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
  let e = Parser.fichier Lexer.token lb in
  if !parse_only then begin
    if !notAffiche then print_endline !file
    else print_endline (show_fichier e); (* On peut switch entre afficher le nom (ie. compil réussie) et afficher l'arbre généré *)
    exit 0;
    end
  else begin
    print_endline (show_fichier e);
    print_endline !file;
    exit 0;
  end
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
