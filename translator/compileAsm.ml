(* bytecode generation from program *)
open SimpleAsm 
open Printf
exception BytecodeGenError of int * string

module List = struct
  include List
  let iteri f = 
    let i = ref 0 in
    List.iter (fun x -> f !i x; incr i)
end

let generate (codes: SimpleAsm.bytecmd list) : int list = 
  let module SM = Map.Make (String) in
  let labels,_ = 
    List.fold_left (fun (acc,i) cmd -> match cmd with
      | Label s -> 
          (SM.add s i acc,i)
      | _ -> (acc,i+ SimpleAsm.lengther cmd)
    ) (SM.empty,0) codes
  in
  let ans = ref [] in
  let add x = ans := x :: !ans in
  let cori = Types.code_of_regI in
  let jumpHelper pos cmd =
    let helper code s =
      try let target = SM.find s labels in
          add code; add target
      with Not_found ->
        raise (BytecodeGenError (pos, sprintf "Label `%s` not found" s))
    in
    match cmd with
      | JumpLess s -> helper 46 s
      | JumpEq   s -> helper 42 s
      | JumpGre  s -> helper 44 s
      | _ -> assert false
  in
  let f pos cmd = match cmd with
    | Mov1 (l,r) -> add 7;  add (cori l); add (cori r)
    | Mov2 (x,r) -> add 1;  add x;        add (cori r)
    | Add1 (l,r) -> add 23; add (cori l); add (cori r)
    | Add2 (x,r) -> add 22; add x;        add (cori r)
    | Mul1  l    -> add 31; add (cori l)
    | Sub1 (l,r) -> add 27; add (cori l); add (cori r)
    | Int x      -> add 20; add (Types.code_of_interr x)
    | Cmp1 (x,r) -> add 50; add x;  add (cori r)
    | Cmp2 (l,r) -> add 51; add (cori l); add (cori r)
    | JumpGre _ | JumpEq _
    | JumpLess _ -> jumpHelper pos cmd
    | Label _ -> ()
  in
  List.iteri f codes;
  List.rev !ans
















