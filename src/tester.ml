open Lexer;;
open Parser;;
(* open Ast *)

let c = open_in "test.jl";;
let lb = Lexing.from_channel c;;
let e = fichier token lb;;

print_newline ();;
