open Parser
open Ast

exception Overflowing_integer

let posVide:Ast.position = {ldeb = -1; cdeb = -1; lfin = -1; cfin = -1}

let keywords = Hashtbl.create 12
let words = [("else", ELSE posVide); ("elseif", ELSEIF posVide); ("end", END posVide); ("false", FALSE posVide); ("for", FOR posVide);
  ("function", FUNCTION posVide); ("if", IF posVide); ("mutable", MUTABLE); ("return", RETURN posVide); ("struct", STRUCT posVide);
  ("true", TRUE posVide); ("while", WHILE posVide)]
let () = List.iter (fun (s, t) -> Hashtbl.add keywords s t) words

exception Not_an_int

let separate_int_ident s =
  let n = String.length s in
  let i = ref 0 in
  let found = ref false in
  while !i < n && not !found do
    i := !i + 1;
    match s.[!i] with
      | '0'..'9' | '-' -> ()
      | _ -> found := true;
  done;
  let res1 =
  try Int64.of_string (String.sub s 0 (!i))
  with _ -> raise Overflowing_integer
  in
  (res1, String.sub s (!i) (n - !i))


let canEnd = ref false;;
let dernierEstElse = ref false;;

let enableEnd () =
  dernierEstElse := false;
  canEnd := true
;;

let disableEnd () =
  dernierEstElse := false;
  canEnd := false
;;

let string_stack = ref []

let push_to_string_stack a =
  string_stack := a :: !string_stack
;;

let empty_stack () =
  let b = !string_stack in
  string_stack := [];
  let n = List.length b in
  let buf = Buffer.create n in
  List.iter (Buffer.add_char buf) (List.rev b);
  Buffer.contents buf
;;

let escaped = ref false;;


let parDepth = ref 0;;

let enterPar () =
  parDepth := !parDepth + 1
;;

let leavePar () =
  parDepth := !parDepth - 1;
  !parDepth < 0
;;

let position lb =
  let deb = Lexing.lexeme_start_p lb in
  let fin = Lexing.lexeme_end_p lb in
  {ldeb = deb.pos_lnum; cdeb = deb.pos_cnum - deb.pos_bol; lfin = fin.pos_lnum; cfin = fin.pos_cnum - fin.pos_bol;}

let fusionPos p1 p2 =
  {ldeb = p1.ldeb; cdeb = p1.cdeb; lfin = p2.lfin; cfin = p2.cfin}


let rajoutePosition tk lb =
  let p = position lb in
  match tk with
    | ELSE _ -> ELSE p
    | ELSEIF _ -> ELSEIF p
    | END _ -> END p
    | FALSE _ -> FALSE p
    | FOR _ -> FOR p
    | FUNCTION _ -> FUNCTION p
    | IF _ -> IF p
    | MUTABLE -> MUTABLE
    | RETURN _ -> RETURN p
    | STRUCT _ -> STRUCT p
    | TRUE _ -> TRUE p
    | WHILE _ -> WHILE p
    | _ -> failwith "Unknown keyword"

let file = ref "test.jl"
