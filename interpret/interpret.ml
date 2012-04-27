(* interpreter of byte code *)
open Types
open Printf

let parse = 
  let pos = ref 0 in
  let incr2 () = pos := 2 + !pos in
  let incr3 () = pos := 3 + !pos in
  let wrap2regs l r onOK = 
    match (regI_of_code l,regI_of_code r) with
      | Some l,Some r -> onOK l r
      | _ -> 
          try
            let _ = regI_of_code_exn l
            and _ = regI_of_code_exn r in
            assert false
          with BadCode (c,str) -> 
            fprintf stderr "Error parsing bytecode %d at position %d: \"%s\"" c !pos str;
            exit 0                
  in
  let rec loop acc lst = 
    match lst with 
      | 7 :: l :: r :: tl -> (* move register to a register *)
        wrap2regs l r (fun l r -> 
              incr3 ();
              loop ( (Mov1 (l,r)) :: acc) tl 
        )
      | 23 :: l :: r :: tl -> (* add l to r*)
        wrap2regs l r (fun l r -> 
          incr3 ();
          loop ( (Add1 (l,r)) :: acc) tl 
        )
      | 27 :: l :: r :: tl -> (* substract l from r *)
        wrap2regs l r (fun l r -> 
          incr3 ();
          loop ( (Sub1 (l,r)) :: acc) tl 
        )
      |  20 :: icode :: tl -> begin (* interrupt *)
        match interr_of_code icode with
          | None -> begin
            try ignore (interr_of_code_exn icode); assert false
            with BadCode (c,msg) ->
                fprintf stderr "Error parsing bytecode %d at position %d: \"%s\"" c !pos msg;
                exit 0              
          end
          | Some i -> incr2 (); loop ( (Int i) :: acc) tl 
      end
      | [] -> List.rev acc
      | _ -> begin
        fprintf stderr "Error while interpreting codes. Parsed part:\n";
        List.iter (print_bytecmd stdout) (List.rev acc);
        fprintf stderr "\nTail is:\n";
        List.iter (fprintf stderr "%d ") lst;
        exit 0        
      end
  in 
  loop []

type env = {
  mutable ah : int;
  mutable bh : int;
}
let print_env env = 
  printf "AH=%d\t BH=%d\n" env.ah env.bh

exception End_of_execution
let interpret bytecodes = 
  let program = parse bytecodes in
  let env = { ah=0; bh=0 } in
  let val_of_regI = function
    | AH -> env.ah
    | BH -> env.bh
  in
  let put_reg r x = match r with
    | AH -> env.ah <- x
    | BH -> env.bh <- x
  in
  
  let exec x = match x with
    | Mov1 (l,r) -> (* move from register l ro register r *)
        let x = val_of_regI l in
        put_reg r x
    | Add1 (l,r) -> (* add register l to register r*) 
        let x = val_of_regI l and y = val_of_regI r in
        put_reg r (x+y)
    | Sub1 (l,r) -> (* substract register l from register r *) 
        let x = val_of_regI l and y = val_of_regI r in
        put_reg r (y-x)
    | Int IExit -> ()
    | Int IOutInt -> (* print AH *)
        printf "%d\n" env.ah
    | Int IInputInt  -> (* put integer to AH *)
        Scanf.scanf "%d\n" (fun x -> env.ah <- x)
  in
  let () = 
    try 
      List.iter  exec program;
      printf "interpreting finished"
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




















