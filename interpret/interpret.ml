(* interpreter of byte code *)
open Types
open Printf

let parse (ints: int list) =
  let wrap_reg r =
    match regI_of_code r with
      | Some r -> r
      | None ->
          try let _ = regI_of_code_exn r in assert false
          with BadCode (c,str) -> 
            fprintf stderr "Error parsing bytecode %d: \"%s\"" c str;
            exit 0
  in
  let wrap2regs l r onOK = onOK (wrap_reg l) (wrap_reg r) in
  let ans = Array.create (List.length ints) None in
  let parse_loop_helper lst =
    match lst with 
      | 19 :: tl -> (* NO oPeration *)
          Some (Nop,tl)
      | 1 :: x :: r :: tl -> (* move integer to a register *)
          let r = wrap_reg r in
          Some (Mov2(x,r), tl)
      | 7 :: l :: r :: tl -> (* move register to a register *)
          wrap2regs l r (fun l r -> Some (Mov1 (l,r), tl))
      | 22:: x :: r :: tl -> (* add integer to a register *)
          let r = wrap_reg r in
          Some (Add2(x,r), tl)
      | 23:: l :: r :: tl -> (* add l to r*)
          wrap2regs l r (fun l r -> Some (Add1 (l,r), tl))
      | 27:: l :: r :: tl   -> (* substract l from r *)
          let l,r = wrap_reg l,wrap_reg r in
          Some (Sub1 (l,r),tl)
      | 31:: r :: tl -> (* multiple r to AH *)
          let r = wrap_reg r in
          Some (Mul1 r,tl)
      | 50:: x :: r :: tl -> (* compare integer and register *)
          let r = wrap_reg r in
          Some (Cmp1 (x,r),tl)
      | 51:: l :: r :: tl -> (* compare two registers *)
          Some (Cmp2 (wrap_reg l,wrap_reg r),tl)
      | 20:: icode :: tl  -> begin (* interrupt *)
        match interr_of_code icode with
          | None -> begin
            try ignore (interr_of_code_exn icode); assert false
            with BadCode (c,msg) ->
                fprintf stderr "Error parsing bytecode %d : \"%s\"" c msg;
                exit 0
          end
          | Some i -> Some (Int i, tl)
      end
      | 46 :: x :: tl -> Some (JumpLess x,tl)
      | 42 :: x :: tl -> Some (JumpEq   x,tl)
      | 44 :: x :: tl -> Some (JumpGre  x,tl)
      | _____________ -> None
  in
  let pos = ref 0 in
  let rec parse_loop lst =
    match lst with
      | [] -> ()
      | __ -> begin
        match parse_loop_helper lst with
          | Some (cmd, tl) ->
              ans.(!pos) <- Some cmd;
              pos := (Types.instr_length cmd) + !pos;
              parse_loop tl
          | None  ->
              fprintf stderr "Error while interpreting codes. Parsed part:\n";
              Array.iter (function Some cmd -> print_bytecmd stderr cmd | None -> ()) ans;
              fprintf stderr "\nTail is:\n";
              List.iter (fprintf stderr "%d ") lst;
              exit 0
      end
  in 
  parse_loop ints;
  ans
(**************************************************************************)
type exec_result =
  | Next of bytecmd * int list
  | Jump of int
  | Error

type env = {
  mutable ah : int;
  mutable bh : int;
  mutable ch : int;
  mutable dh : int;
  mutable eh : int;  mutable sp : int;
}
let print_env env = 
  printf "{AH=%d, BH=%d, CH=%d, DH=%d, EH=%d, SP=%d}\n" env.ah env.bh env.ch env.dh env.eh env.sp

exception End_of_execution
let interpret bytecodes = 
  let program = parse bytecodes in
  let env = { ah=0; bh=0; ch=0; dh=0; eh=0; sp=0 } in
  let val_of_regI = function
    | AH -> env.ah
    | BH -> env.bh
    | CH -> env.ch
    | DH -> env.dh
    | EH -> env.eh
    | SP -> env.sp
  in
  let put_reg r x = match r with
    | AH -> env.ah <- x
    | BH -> env.bh <- x
    | CH -> env.ch <- x
    | DH -> env.dh <- x
    | EH -> env.eh <- x
    | SP -> env.sp <- x
  in
  let shift ~cmd = env.sp <- env.sp + (Types.instr_length cmd) in
  let shift' x =   env.sp <- x in
  let exec cmd = match cmd with
    | Nop        -> shift' 1
    | Mov1 (l,r) -> (* move from register l ro register r *)
        put_reg r (val_of_regI l);
        shift ~cmd
    | Mov2 (x,r) ->  (* put integer to a register *)
        put_reg r x;
        shift ~cmd
    | Add1 (l,r) -> (* add register l to register r*) 
        let x = val_of_regI l and y = val_of_regI r in
        put_reg r (x+y);
        shift ~cmd
    | Add2 (x,r) -> (* add integer to a register *)
        let y = val_of_regI r in
        put_reg r (x+y);
        shift ~cmd
    | Mul1 r    -> (* muliply r to AH and put to AH *)
        let x = val_of_regI r and y = val_of_regI AH in
        put_reg AH (x*y);
        shift ~cmd
    | Sub1 (l,r) -> (* substract register l from register r *) 
        let x = val_of_regI l and y = val_of_regI r in
        put_reg r (y-x);
        shift ~cmd
    | JumpLess addr ->
        shift' (if env.eh = -1 then addr else env.sp+(instr_length cmd))
    | JumpEq   addr ->
        shift' (if env.eh =  0 then addr else env.sp+(instr_length cmd))
    | JumpGre  addr ->
        shift' (if env.eh =  1 then addr else env.sp+(instr_length cmd))
    | Cmp1 (x,r) -> 
        let r = val_of_regI r in
        put_reg EH (compare x r)
    | Cmp2 (l,r) ->
        let l = val_of_regI l and r = val_of_regI r in
        put_reg EH (compare l r);
        shift ~cmd
    | Int IExit -> assert false
    | Int IOutInt -> (* print AH *)
        printf "%d\n" env.ah;
        shift ~cmd
    | Int IInputInt -> (* put integer to AH *)
        Scanf.scanf "%d\n" (fun x -> env.ah <- x);
        shift ~cmd
  in
  let rec exec_loop () =
    let miss_sp () = 
      printf "No command on this position %d\n" env.sp
    in
    if env.sp >= Array.length program then miss_sp ()
    else match program.(env.sp) with
      | None -> miss_sp ()
      | Some (Int IExit) -> ()
      | Some instr ->
          exec instr;
          exec_loop ()
  in
  let () = 
    env.sp <- 0;
    try 
      if Array.length program = 0
      then printf "Program is empty\n"
      else exec_loop ();
      printf "Interpreting finished\n"
    with End_of_execution -> printf "Fatal error while execution\n"
  in 
  print_env env
      
let filename = "program.ve"
let readfile filename =
  let lst = ref [] in
  let ch = open_in filename in
  try
    while true do
      lst := (int_of_char (input_char ch)) :: !lst 
    done;
    assert false
  with End_of_file -> 
    close_in ch;
    List.rev !lst

    
let () = 
  let bytecodes = readfile filename in
  interpret bytecodes




















