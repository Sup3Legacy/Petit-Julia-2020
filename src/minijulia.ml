open Lexer
open Parser
open Dum
open Ast
(* open Ast *)

let file = ref "test.jl";;
let notAffiche = ref true;;

let handle () =
  let c = open_in !file in
  let lb = Lexing.from_channel c in
  let e = Parser.fichier Lexer.token lb  in
  if !notAffiche then print_endline !file
  else print_endline (show_fichier e) (* On peut switch entre afficher le nom (ie. compil réussie) et afficher l'arbre généré *)
;;



let set_filename n =
  file := n
;;


let main () =
  begin
    let speclist = [("-f", Arg.String(set_filename), "file to process.");
    ("-disable_print", Arg.Set notAffiche, "remove the print of the abstract.");
    ] in
    Arg.parse speclist (fun x -> ()) "Issouffle";
    handle ();
  end
;;

let () = main ();;
