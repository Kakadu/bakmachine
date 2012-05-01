open Comb
open SimpleAsm
open Sexplib.Conv 
open Printf

let comma = p_char ','

let pair p1 p2 : _ -> (_*_) parse_result =
  p1 >>> p_space >>= (fun l -> 
    (p2 >>= (fun r stream -> 
      Parsed ((l,r),stream) )
    )
  )

let pregister =
  let open Types in
  let h s r = pstring s >>= (fun _ tl -> Parsed(r,tl) ) in  
  (
    h "ah" AH <|> 
    h "bh" BH <|>
    h "ch" CH <|>
    h "dh" DH <|>
    h "eh" EH
  ) >>= (fun r s -> Parsed (r,s))

let sexp_of_pres = sexp_of_parse_result sexp_of_char 
let label_good_char c = (c>='a' && c<='z') || (c>='0' && c<='9')
let p_label stream =
  let module B=Buffer in
  let b = B.create 10 in
  let rec loop s = 
    match s with
    | x   :: tl when label_good_char x  -> B.add_char b x; loop tl
    | ':' :: tl when B.length b>0 -> 
        Parsed (B.contents b, tl)
    | _ -> Failed
  in
  loop stream

let lines =
  let label' = (p_label >>> p_space) >>= (fun s r ->  Parsed(Label s,r) ) in
  let mov' = 
    (pstring "mov") >>. p_space >>. (
      (pair pregister  pregister >>= (fun (l,r) s -> Parsed(Mov1(l,r),s) )) <|>
      (pair p_uinteger pregister >>= (fun (l,r) s -> Parsed(Mov2(l,r),s) )) 
    ) in
  let cmp' =
    (pstring "cmp") >>. p_space >>. (
      (pair p_uinteger pregister >>= (fun (l,r) s -> Parsed(Cmp1 (l,r),s) ) ) <|>
      (pair pregister  pregister >>= (fun (l,r) s -> Parsed(Cmp2 (l,r),s) ) ) 
    ) in
  let sub' = 
    (pstring "sub") >>. p_space >>. (
      pair pregister pregister >>= (fun (l,r) s -> Parsed(Sub1(l,r),s) )
    ) in
  let add' = 
    (pstring "add") >>. p_space >>. (
      (pair pregister  pregister >>= (fun (l,r) s -> Parsed(Add1 (l,r),s) )) <|>
      (pair p_uinteger pregister >>= (fun (x,r) s -> Parsed(Add2 (x,r),s) ))
    ) in
  let mul' = 
    (pstring "mul") >>. p_space >>. (
      pregister >>= (fun r s -> Parsed (Mul1 r,s) )
    ) in 
  let jl = 
    (pstring "jl") >>. p_space >>. (
      min1manyCharsLike label_good_char >>= (fun r s -> Parsed (JumpLess r,s))
     ) in
  let inter' =
    (pstring "int") >>. p_space >>. p_uinteger >>= (fun r s -> 
      match Types.inter_of_int r with
        | Some r -> Parsed (Int r,s) 
        | None   -> Failed
    )
  in
  let cmds = mov' <|> inter' <|> add' <|> sub' <|> cmp' <|> mul' <|> jl <|> label' in

  p_manyf 
    (cmds >>> p_endline) 
    (fun x y -> y::x) [] >>= (fun ans s -> Parsed (List.rev ans,s) )








