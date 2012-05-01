open Sexplib.Conv

let (|>) x f = f x
type regI = AH|BH|CH|DH|EH (*|EAX | BP | SP*) 
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
  | Add2 of int  * regI
  | Mul1 of regI        (* multiple reg to AH and put to AH *)
  | Sub1 of regI * regI
  | JumpLess of int
  | Cmp1 of int  * regI (* compare left operand with right *)
  | Cmp2 of regI * regI (* puts -1/0/1 to EH if left is less/eq/more than right *)
  | Int of interrupt
  | Nop
with sexp

let instr_length = function
  | Mov1 _ | Mov2 _ | Add1 _ | Add2 _ | Sub1 _ | Cmp1 _ 
  | Cmp2 _ -> 3
  | Int _ | JumpLess _ 
  | Mul1 _ -> 2
  | Nop  -> 1

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
      | Add2 (a,b) -> sprintf "add %d,%s" a        (sreg b)
      | Mul1   r   -> sprintf "mul %s"    (sreg r)
      | JumpLess x -> sprintf "jl %d" x
      | Sub1 (a,b) -> sprintf "sub %s,%s" (sreg a) (sreg b)
      | Cmp1 (x,r) -> sprintf "cmp %d,%s" x        (sreg r)
      | Cmp2 (a,b) -> sprintf "cmp %s,%s" (sreg a) (sreg b)
      | Nop        -> "nop"
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
  | CH -> 2
  | DH -> 3
  | EH -> 4

let regI_of_code_exn c = 
  try 
    let ans = match c with
      | 0 -> AH
      | 1 -> BH 
      | 2 -> CH 
      | 3 -> DH
      | 4 -> EH
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


















