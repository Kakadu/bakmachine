(* in this project translator = compiler *)
open Printf
let filename = "input.asm"

let stream =  
  let chan = open_in filename in
  let ans = ref [] in
  try
    while true do
      ans := (input_char chan) :: !ans
    done;
    List.rev !ans
  with End_of_file -> 
    close_in chan;
    List.rev !ans

let code =
  match Parser.lines stream with
    | Comb.Parsed (lst,_) ->
        Printf.printf  "parsed %d operations!\n" (List.length lst);
        lst
    | Comb.Failed -> 
        print_endline "parsing failed";
        exit 0

open Sexplib.Conv

let (|>) x f = f x
let () = Types.print_prog stdout code

let bytecode = Genbyte.generate code
let () = 
  print_endline "bytecode:";
  List.iter (printf "%d ") bytecode;
  print_endline ""

let () = 
  let out_file = "program.ve" in
  let ch = open_out out_file in
  List.iter (fun x -> Printf.fprintf ch "%c" (char_of_int x) ) bytecode;
  close_out ch























