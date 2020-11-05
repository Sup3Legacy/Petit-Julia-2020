open Parser

exception Overflowing_integer

let keywords = Hashtbl.create 12
let words = [("else", ELSE); ("elseif", ELSEIF); ("end", END); ("false", FALSE); ("for", FOR);
  ("function", FUNCTION); ("if", IF); ("mutable", MUTABLE); ("return", RETURN); ("struct", STRUCT);
  ("true", TRUE); ("while", WHILE)]
let () = List.iter (fun (s, t) -> Hashtbl.add keywords s t) words

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
  try int_of_string (String.sub s 0 (!i))
  with _ -> raise Overflowing_integer
  in
  (res1, String.sub s (!i) (n - !i))


let canEnd = ref false;;

let enableEnd () =
  canEnd := true
;;

let disableEnd () =
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
