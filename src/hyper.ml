open Parser

exception Overflowing_integer

let keywords = Hashtbl.create 12
let words = [("else", ELSE); ("elseif", ELSEIF); ("end", END); ("false", FALSE); ("for", FOR);
  ("function", FUNCTION); ("if", IF); ("mutable", MUTABLE); ("return", RETURN); ("struct", STRUCT);
  ("true", TRUE); ("while", WHILE)]
let () = List.iter (fun (s, t) -> Hashtbl.add keywords s t) words

exception Not_an_int


let int_from_string s =
  let val0 = int_of_char '0' in
  let length = String.length s in
  if length = 0 then raise Not_an_int;
  let n = ref (if s.[0] = '-' then 0 else int_of_char s.[0] - val0) in
  for i = 1 to length-1 do
    n := 10 * !n + int_of_char s.[i] - val0;
  done;
  if s.[0] = '-' then - !n else !n;;

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
  try int_from_string (String.sub s 0 (!i))
  with _ -> raise Overflowing_integer
  in
  (res1, String.sub s (!i) (n - !i))


let canEnd = ref false;;
let dernierEstElse = ref false;;
let position = ref None;;

let enableEnd (lb:Lexing.lexbuf) =
  position := Some lb;
  dernierEstElse := false;
  canEnd := true
;;

let disableEnd lb =
  position := Some lb;
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
