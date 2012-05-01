(* types for a simple assemler language *)
open Sexplib.Conv 
open Printf

type bytecmd =
  | Mov1 of Types.regI * Types.regI  (* move register to register *)
  | Mov2 of int  * Types.regI        (* move integer to register *)
  | Add1 of Types.regI * Types.regI
  | Add2 of int        * Types.regI
  | Mul1 of Types.regI               (* multiple reg to AH and put to AH *)
  | Sub1 of Types.regI * Types.regI
  | Cmp1 of int  * Types.regI        (* compare left operand with right *)
  | Cmp2 of Types.regI * Types.regI  (* puts -1/0/1 to EH if left is less/eq/more than right *)
  | Label of string 
  | JumpLess of string 
  | Int of Types.interrupt

let print_bytecmd ch line = 
  let sreg x = string_of_sexp (Types.sexp_of_regI x) in
  fprintf ch "%s\n" (
    match line with
      | Int Types.IExit -> "interrupt exit"
      | Int Types.IOutInt -> "intterrupt output AH"
      | Int Types.IInputInt -> "interrupt input to AH"
      | Mov1 (a,b) -> sprintf "mov %s,%s" (sreg a) (sreg b)
      | Mov2 (x,r) -> sprintf "mov %d,%s" x        (sreg r)
      | Add1 (a,b) -> sprintf "add %s,%s" (sreg a) (sreg b)
      | Add2 (x,b) -> sprintf "add %d,%s" x        (sreg b)
      | Mul1 a     -> sprintf "mul %s"    (sreg a)
      | Sub1 (a,b) -> sprintf "sub %s,%s" (sreg a) (sreg b)
      | Cmp1 (x,r) -> sprintf "cmp %d,%s" x        (sreg r)
      | Cmp2 (a,b) -> sprintf "cmp %s,%s" (sreg a) (sreg b)
      | Label s    -> sprintf "%s:" s
      | JumpLess s -> sprintf "jl %s" s
  )

let print_prog ch = List.iter (print_bytecmd ch)

let lengther cmd = match cmd with
  | Sub1 _ 
  | Mov1 _ | Mov2 _ | Add1 _ | Add2 _ | Cmp1 _ | Cmp2 _ -> 3
  | Mul1 _ | Int _  | JumpLess _ -> 2
  | Label _ -> 0
