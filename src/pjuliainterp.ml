open Lexer
open Parser
open Ast
open Hyper
open Utilities


let continue = ref true;;
let instr = ref "";;

let startswith str motif =
  let n = String.length str in
  let m = String.length motif in
  if m > n then false
  else
    begin
      let i = ref 0 in
      while !i < m && str.[!i] = motif.[!i] do
        i := !i + 1;
      done;
    !i = n
    end
;;


while !continue do
  (* boucle principale *)
  instr := read_line();
  if startswith
  let lb = Lexing.from_string !instr in
  let e = clean_file (Parser.fichier Lexer.token lb) in
  print_endline (show_fichier e);
done;
