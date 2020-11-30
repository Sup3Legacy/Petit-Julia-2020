
open Lexing
open Ast
open Format

let rec pp_expr fmt = function
	| Int i -> Format.fprintf fmt "%i" i
	| Add (a, b) -> Format.fprintf fmt "%a+%a" pp_expr a pp_expr b
	| Mul (a, b) -> Format.fprintf fmt "(%a)*(%a)" pp_expr a pp_expr b
;;

let f = open_in "fichier.txt" in 
let buf = Lexing.from_channel f in
let parsed = Parser2.parse Lexer2.token buf in
Format.printf "parsed = %a\n" pp_expr parsed;
close_in f;
