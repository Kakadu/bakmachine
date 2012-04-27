open Comb
open Types
open Sexplib.Conv 
open Printf

let comma = p_char ','
let pair p1 p2 : _ -> _ parse_result =
  p1 >>= (fun l stream ->
    match (comma >>. p2) stream with
      | Parsed (r,stream) -> Parsed ((l,r),stream)
      | Failed -> Failed    
  )

let pmov = pstring "mov"
let padd = pstring "add"
let psub = pstring "sub"
let pint = pstring "int"
let pregister =
  printf "inside pregister: \n"; 
  let h s r = pstring s >>= (fun _ s -> Parsed(r,s)) in 
  (
   ( (*
    h "eax" EAX <|>
    h "bp" BP <|> 
        h "sp" SP <|> *)
    h "ah" AH <|> 
    h "bh" BH)
  ) >>= (fun r s -> 
(*    printf "pregister says %s\n" (r |> sexp_of_regI |> string_of_sexp); *)
    Parsed (r,s)
  )

let sexp_of_pres = sexp_of_parse_result sexp_of_char 

let lines =
  let mov' = 
    pmov >>. p_space >>. (
      pair pregister pregister >>= (fun (l,r) s -> Parsed(Mov1(l,r),s) )
    ) in
  let sub' = 
    psub >>. p_space >>. (
      pair pregister pregister >>= (fun (l,r) s -> Parsed(Sub1(l,r),s) )
    ) in
  let add' = 
    padd >>. p_space >>. (
      pair pregister pregister >>= (fun (l,r) s -> Parsed(Add1(l,r),s) )
    ) in

  let inter' =
    pint >>. p_space >>. p_uinteger >>= (fun r s -> 
      Printf.printf "here: %d\n" r;
      match inter_of_int r with
        | Some r ->  Parsed (Int r,s) 
        | None -> Failed
    )
  in
  p_manyf 
    ((mov' <|> inter' <|> add' <|> sub') >>> p_endline >>> p_space) 
    (fun x y -> y::x) [] >>= (fun ans s -> Parsed (List.rev ans,s) )




















