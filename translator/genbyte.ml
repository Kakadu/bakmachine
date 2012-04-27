(* bytecode generation from program *)
open Types
 
let generate codes = 
  let ans = ref [] in
  let add x = ans := x :: !ans in
  let f = function
    | Mov1 (l,r) -> add 7;  add (code_of_regI l); add (code_of_regI r)
    | Add1 (l,r) -> add 23; add (code_of_regI l); add (code_of_regI r)
    | Sub1 (l,r) -> add 27; add (code_of_regI l); add (code_of_regI r)
    | Int x      -> add 20; add (code_of_interr x)
  in
  List.iter f codes;
  List.rev !ans










