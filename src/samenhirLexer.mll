{
(*
###########################################
#                                         #
#             Lexer de Samenhir           #
#                                         #
#     Il compte mal les lignes car il     #
# comprend le code Ocaml comme une string #
#                                         #
###########################################
*)
	open Lexing
	open SamenhirParser
	open SamenhirAst	

  let code_buffer = Buffer.create 1024

}

let chiffre = ['0'-'9']
let alpha = ['a'-'z']|['A'-'Z']|'_'
let chaine = (alpha)(alpha | chiffre)*
let maj = ['A'-'Z']
let min = ['a'-'z']
let term = (maj)(alpha | chiffre)*
let notTerm = (min)(alpha | chiffre)*
let chaineCode = [^'%']
let chaineType = [^'>']
let space = " " | "\t"
rule token = parse 
	| "<" (chaineType* as c) ">" { TYPE c}
	| "=" {ASSOC}
	| "%{" (chaineCode* as c) "%}" { HEADER c}
	| "{" {let _ = code lexbuf in let s = Buffer.contents code_buffer in Buffer.reset code_buffer; CODE s}
	| notTerm as nt { N_TERM_IDENT nt}
	| term as t {TERM t}
	| ":" {POINTS}
	| ";" {SEMICOLON}
	| space {token lexbuf}	
	| "\n" {new_line lexbuf; token lexbuf}
	| "%%" {SPLIT}
	| "%token" {TOKEN}
	| "%left" {LEFT}
	| "%right" {RIGHT}
	| "%start" {START}
	| "%nonassoc" {NONASSOC}
	| "%prec" {PREC}
	| "|" {VERT}
	| "/*" {comment lexbuf}
	| eof {EOF}
	| (_ as c) {print_char '\"';print_char c;print_char '\"';print_newline ();failwith "unknown char"}
and comment = parse 
	| "*/" {token lexbuf}
	| _ {comment lexbuf}
and code = parse
	| '}' { CODE ""}
	| '{' {
		Buffer.add_char code_buffer '{';
		let _ = code lexbuf in
		Buffer.add_char code_buffer '}';
		code lexbuf
	}
	| '\n' {Buffer.add_char code_buffer '\n'; new_line lexbuf; code lexbuf}
	| _ as c {Buffer.add_char code_buffer c; code lexbuf}
