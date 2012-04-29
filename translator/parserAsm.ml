open Comb
(* open Types *)
open SimpleAsm
open Sexplib.Conv 
open Printf

let comma = p_char ','
let pair p1 p2 : _ -> (_*_) parse_result =
  p1 >>= (fun l stream ->
    match (comma >>. p2) stream with
      | Parsed (r,stream) -> Parsed ((l,r),stream)
      | Failed -> Failed    
  )

let pmov = pstring "mov"
let padd = pstring "add"
let pint = pstring "int"

let pregister =
  let open Types in
  let h s r = pstring s >>= (fun _ s -> Parsed(r,s)) in 
  (
   ( (*
    h "eax" EAX <|>
    h "bp" BP <|>  
    h "sp" SP <|>  *)
    h "ah" AH <|> 
    h "bh" BH <|>
    h "ch" CH <|>
    h "dh" DH <|>
    h "eh" EH 
   ) 
  ) >>= (fun r s -> Parsed (r,s) )

let sexp_of_pres = sexp_of_parse_result sexp_of_char 
let p_label stream =
  let module B=Buffer in
  let b = B.create 10 in
  let good_char c = (c>='a' && c<='z') || (c>='0' && c<='9') in
  let rec loop s = 
    printf "contents: `%s`, length = %d\n" (B.contents b) (B.length b);
    match s with
    | x   :: tl when good_char x  -> B.add_char b x; loop tl
    | ':' :: tl when B.length b>0 -> 
        printf "label finised with `%s`. tails length = %d\n" (B.contents b) (List.length tl);
        Parsed (B.contents b, tl)
    | _ -> print_endline "loop says failed" ; Failed
  in
  loop stream

let lines =
  let label' = (p_label >>> p_space) >>= (fun s r -> 
    print_endline "olololo";
    Parsed(Label s,r) ) in
  let mov' = 
    pmov >>. p_space >>. (
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
    padd >>. p_space >>. (
      pair pregister pregister >>= (fun (l,r) s -> Parsed(Add1 (l,r),s) )
    ) in
  let inter' =
    (pstring "int") >>. p_space >>. p_uinteger >>= (fun r s -> 
      Printf.printf "here: %d\n" r;
      match Types.inter_of_int r with
        | Some r ->  Parsed (Int r,s) 
        | None -> Failed
    )
  in
  let cmds = mov' <|> inter' <|> add' <|> sub' <|> cmp' <|> label' in
(*  let cmds = (* mov' <|> inter' <|> add' <|> sub' <|> cmp' <|> *) label' in *)
  p_manyf 
    (cmds >>> p_endline) 
    (fun x y -> print_endline "new line parsed"; y::x) [] >>= (fun ans s -> Parsed (List.rev ans,s) )








