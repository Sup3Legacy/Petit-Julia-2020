open Parser

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
      | '0'..'9' -> ()
      | _ -> found := true;
  done;
  (int_of_string (String.sub s 0 (!i)), String.sub s (!i) (n - !i))


let canEnd = ref false;;

let enableEnd () =
  canEnd := true
;;

let disableEnd () =
  canEnd := false
;;
