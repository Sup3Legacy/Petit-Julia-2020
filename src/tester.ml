#use "./_build/default/lexer.ml";;
#use "./_build/default/parser.ml";;
(* open Ast *)

let c = open_in "test.jl";;
let lb = Lexing.from_channel c;;
let e = fichier token lb;;

print_newline ();;
