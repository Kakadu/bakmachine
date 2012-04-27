open Sexplib.Conv
open Printf
let (|>) x f = f x

type 'a parse_result = 
  | Parsed of 'a * char list
  | Failed
with sexp
let p_pred p s = match s with 
  | [] -> Failed 
  | h::t -> if p h then Parsed(h, t) else Failed

let p_char c = p_pred ((=) c) 
let p_endline = p_char '\n'
let isdigit c = c>='0' && c<='9' 
let p_digit = p_pred isdigit

let p_info n p s =
  let rec loop n s =
    match s with
      | _ when n<=0 -> Parsed((),[])
      | []          -> Parsed((), s)
      | x::xs when n>0 -> 
          if x='\n' then print_string "\\n" else print_char x;
          loop (n-1) xs
      | _::_ ->  assert false
  in
  printf "p_info says:\n`";
  ignore (loop n s);
  printf "`";
  flush stdout;
  p s

let (>>.) a b s = 
  match a s with 
    | Parsed(_, s2) -> b s2 
    | Failed -> Failed

let ( >>> ) p1 p2 s =
  match p1 s with
    | Parsed (ans,s2) -> begin
      match p2 s2 with
        | Parsed (_,s3) -> Parsed (ans,s3)
        | Failed -> Failed
    end
    | Failed -> Failed

let ( <|> ) a b s = 
  match a s with 
    | Parsed _ as ok -> ok 
    | Failed -> b s

let return x s = Parsed(x, s)
let (>>=) p1 f s = 
  match p1 s with 
    | Parsed(x, s2) -> f x s2 
    | Failed -> Failed

let pstring s stream = (*
  let ppp s stream =  *)
  let len = String.length s in
  let rec loop i stream =
    if i=len then (
(*      printf "p_string succesful for string `%s`\n" s; *)
      Parsed (s,stream)  )
    else
    match p_char s.[i] stream with
      | Failed -> Failed
      | Parsed (_,s2) -> loop (i+1) s2
  in
  loop 0 stream (*
 in
 p_info 10 (ppp s) stream *)

let print_stream s = 
  print_char '`';
  List.iter (print_char) s;
  print_endline "`"

let p_manyf a f v0 = 
  let rec loop v s = 
    printf "inside p_manyf.loop: %s\n" "";
    print_stream s;
    match a s with 
      | Parsed(x, s') -> loop (f v x) s' 
      | Failed -> 
          printf "here =(\n";
          Parsed(v, s) 
  in loop v0

let p_space =
  (p_manyf (p_char ' ' <|> p_char '\t' <|> p_char '\n') (fun l _ -> l) "") >>= (fun _ s -> Parsed((),s))


let mkInt v x = 
  let ans = v * 10 + int_of_char x - 48 in
  printf "mkInt of %d %c says %d\n" v x ans;
  ans

let p_uinteger =
  p_manyf p_digit mkInt 0 >>= (fun r s -> printf "uinteger says %d\n" r; Parsed (r,s) )
  


















