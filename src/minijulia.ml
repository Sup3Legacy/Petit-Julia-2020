open Lexer
open Parser
open Dum
open Ast
(* open Ast *)

let file = ref "test.jl";;
let affiche = false;;

let handle () =
  let c = open_in !file in
  let lb = Lexing.from_channel c in
  let e = Parser.fichier Lexer.token lb  in
  if affiche then print_endline (show_fichier e)
  else print_endline !file (* On peut switch entre afficher le nom (ie. compil réussie) et afficher l'arbre généré *)
;;



let set_filename n =
  file := n
;;


let main () =
  begin
    let speclist = [("-f", Arg.String(set_filename), "file to process.");] in
    Arg.parse speclist (fun x -> ()) "Issouffle";
    handle ();
  end
;;

let () = main ();;
