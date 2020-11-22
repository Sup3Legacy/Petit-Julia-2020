open Lexer
open Parser
open Typer
open Ast
open Astype
open Hyper
open Utilities
open Interp


let continue = ref true;;
let instr = ref "";;

let gVenv = ref (Tmap.singleton "nothing" Nothing)
let gFenv = ref (Tmap.singleton "div" [[Int64; Int64], Int64])
let gSenv = ref (Tmap.empty : structEnv)
let gAenv = ref (Tmap.empty : argsEnv)

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
    !i = m
    end
;;


while !continue do
  (* boucle principale *)
  print_newline();
  print_string "λ>";
  instr := read_line();
  let lb =
    if startswith !instr "#run"
      then
        begin
          let n = String.length !instr in
          print_int n;
          print_endline (String.sub !instr 5 (n - 5));
          let file = open_in (String.sub !instr 5 (n - 5)) in
          Lexing.from_channel file
        end
      else
        Lexing.from_string !instr
  in
  let e = clean_file (Parser.fichier Lexer.token lb) in
  let () = Typer.verificationType e gVenv gFenv gSenv gAenv in
  (* print_endline (show_fichier e); *)
  interp_file e globVenv globFenv globSenv;
done;
