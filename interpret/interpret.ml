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
      | 48 :: x :: tl -> Some (JumpLess x,tl)
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
  mutable eh : int;
}
let print_env env = 
  printf "{AH=%d, BH=%d, CH=%d, DH=%d, EH=%d}\n" env.ah env.bh env.ch env.dh env.eh

exception End_of_execution
let interpret bytecodes = 
  let program = parse bytecodes in
  let env = { ah=0; bh=0; ch=0; dh=0; eh=0 } in
  let val_of_regI = function
    | AH -> env.ah
    | BH -> env.bh
    | CH -> env.ch
    | DH -> env.dh
    | EH -> env.eh
  in
  let put_reg r x = match r with
    | AH -> env.ah <- x
    | BH -> env.bh <- x
    | CH -> env.ch <- x
    | DH -> env.dh <- x
    | EH -> env.eh <- x
  in

  let next_instr prev cmd = match cmd with
    | JumpLess x -> x
    | _  -> prev + (instr_length cmd) in

  let exec x = match x with
    | Nop        -> ()
    | Mov1 (l,r) -> (* move from register l ro register r *)
        let x = val_of_regI l in
        put_reg r x
    | Mov2 (x,r) ->  (* put integer to a register *)
        put_reg r x
    | Add1 (l,r) -> (* add register l to register r*) 
        let x = val_of_regI l and y = val_of_regI r in
        put_reg r (x+y)
    | Add2 (x,r) -> (* add integer to a register *)
        let y = val_of_regI r in
        put_reg r (x+y)
    | Mul1 r    -> (* muliply r to AH and put to AH *)
        let x = val_of_regI r and y = val_of_regI AH in
        put_reg AH (x*y)
    | Sub1 (l,r) -> (* substract register l from register r *) 
        let x = val_of_regI l and y = val_of_regI r in
        put_reg r (y-x)
    | JumpLess _ -> ()
    | Cmp1 (x,r) -> 
        let r = val_of_regI r in
        put_reg EH (compare x r)
    | Cmp2 (l,r) ->
        let (l,r) = val_of_regI l, val_of_regI r in
        put_reg EH (compare l r)
    | Int IExit -> ()
    | Int IOutInt -> (* print AH *)
        printf "%d\n" env.ah
    | Int IInputInt -> (* put integer to AH *)
        Scanf.scanf "%d\n" (fun x -> env.ah <- x)
  in
  let rec exec_loop pos =
    match program.(pos) with
      | None -> exec_loop (pos+1)
      | Some (Int IExit) -> ()
      | Some instr ->
          print_env env;
          printf "exec instruction at pos %d: " pos;
          Types.print_bytecmd stdout instr;
          exec instr;
          exec_loop (next_instr pos instr)
  in
  let () = 
    try 
      if Array.length program = 0
      then printf "Program is empty\n"
      else exec_loop 0;
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




















