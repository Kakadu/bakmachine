open Sexplib.Conv

type regI = AH | BH (*|EAX | BP | SP*) 
with sexp
(** integer registers *)
(*type ram = Addr of int with sexp    *)
type interrupt =
  | IExit
  | IOutInt
  | IInputInt      
with sexp
type bytecmd =
  | Mov1 of regI * regI (* move register to register *)
  | Mov2 of int  * regI (* move integer to register *)
  | Add1 of regI * regI
  | Sub1 of regI * regI
(*  | Label of string *)
  | Int of interrupt
with sexp

let inter_of_int = function
  | 10 -> Some IExit
  | 11 -> Some IOutInt
  | 13 -> Some IInputInt
  | _ -> None

open Printf
let print_bytecmd ch line = 
  let sreg x = string_of_sexp (sexp_of_regI x) in
  fprintf ch "%s\n" (
    match line with
      | Int IExit -> "interrupt exit"
      | Int IOutInt -> "intterrupt output AH"
      | Int IInputInt -> "interrupt input to AH"
      | Mov1 (a,b) -> sprintf "mov %s,%s" (sreg a) (sreg b)
      | Mov2 (x,r) -> sprintf "mov %d,%s" x        (sreg r)
      | Add1 (a,b) -> sprintf "add %s,%s" (sreg a) (sreg b)
      | Sub1 (a,b) -> sprintf "sub %s,%s" (sreg a) (sreg b)
(*      | _ -> 
          flush stdout;
          assert false *)
    )

let print_prog ch lst =  
  List.iter (print_bytecmd ch) lst

exception BadCode of int * string
let code_of_interr = function
  | IExit -> 10
  | IInputInt ->  12
  | IOutInt -> 11

let interr_of_code_exn code = 
 try
   let ans = match code with
     | 10 -> IExit
     | 12 -> IInputInt
     | 11 -> IOutInt
     | _ -> raise (BadCode (code, "error while parsing interrupt code"))
   in
   assert (code_of_interr ans = code);
   ans
 with exc -> raise exc
let interr_of_code code =
  try Some (interr_of_code_exn code)
  with BadCode _ -> None

let code_of_regI = function
  | AH -> 0
  | BH -> 1

let regI_of_code_exn c = 
  try 
    let ans = match c with
      | 0 -> AH
      | 1 -> BH 
      | _ -> raise (BadCode (c,"while trying to parse register"))
    in
    assert (c = code_of_regI ans);
    ans
  with exc -> raise exc

let regI_of_code c = 
  try Some (regI_of_code_exn c)
  with BadCode _ -> None
 
module Option = struct
  type 'a monad = 'a option
  let return x = Some x
  let bind f x = match x with
    | Some x -> Some (f x)
    | None   -> None
  let (>>=) x f = bind f x
end


















