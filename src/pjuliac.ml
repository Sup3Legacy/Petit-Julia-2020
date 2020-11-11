open Lexer
open Parser
open Dum
open Ast
(* open Ast *)

let notAffiche = ref false;;
let parse_only = ref false;;
let type_only = ref false;;

let handle () =
  let c = open_in !file in
  let lb = Lexing.from_channel c in
  let e = (try 
    Parser.fichier Lexer.token lb
    with  _ -> begin
          let b = Lexing.lexeme_start_p lb in 
          let e = Lexing.lexeme_end_p lb in 
          let s = Lexing.lexeme lb in
          Printf.printf "File \"%s\", line %d, character %d-%d :\n" !file b.pos_lnum (b.pos_cnum - b.pos_bol) (e.pos_cnum - e.pos_bol);
          Printf.printf "Syntax error at lexeme : \"%s\"\n" s;
          exit 1       
        end)
    in
  if !parse_only then begin
    if !notAffiche then print_endline !file
    else print_endline (show_fichier e); (* On peut switch entre afficher le nom (ie. compil réussie) et afficher l'arbre généré *)
    exit 0;
    end;
  print_endline !file;
  exit 0;
;;



let set_filename n =
  file := n
;;


let main () =
  begin
    let speclist = [("-f", Arg.String(set_filename), "file to process.");
    ("-disable_print", Arg.Set notAffiche, "remove the print of the abstract.");
    ("--parse_only", Arg.Set parse_only, "Stop after parsing");
    ("--type_only", Arg.Set type_only, "Stop after typing");
    ] in
    Arg.parse speclist (fun x -> ()) "Issouffle";
    handle ();
  end
;;

let () = main ();;