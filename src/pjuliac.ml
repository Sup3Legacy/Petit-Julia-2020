open Lexer
open Parser
open Ast
open Hyper
open Utilities

let notAffiche = ref false;;
let parse_only = ref true;;
let type_only = ref false;;

let handle () =
  let c = open_in !(Hyper.file) in
  let lb = Lexing.from_channel c in
  try
    let e = clean_file (Parser.fichier Lexer.token lb)
    in
    if !parse_only then begin
      if !notAffiche then print_endline !(Hyper.file)
      else print_endline (show_fichier e); (* On peut switch entre afficher le nom (ie. compil réussie) et afficher l'arbre généré *)
      exit 0;
      end;
    print_endline !(Hyper.file);
    exit 0;
  with a -> begin
      let b = Lexing.lexeme_start_p lb in
      let e = Lexing.lexeme_end_p lb in
      Printf.printf "File \"%s\", line %d, character %d-%d :\n" !(Hyper.file) b.pos_lnum (b.pos_cnum - b.pos_bol) (e.pos_cnum - e.pos_bol);
      match a with
        |Lexer.Lexing_error s -> Printf.printf "Lexical error at lexeme : \"%s\"\n" s
        |Parser.Error -> Printf.printf "Syntax error\n"
        |_ -> Printf.printf "Unkown error\n";
      exit 1
    end
;;



let set_filename n =
  Hyper.file := n
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
