
{

	open Lexing
	open SamenhirAst	
	open Parser2


}

let chiffre = ['0'-'9']
let alpha = ['a'-'z']|['A'-'Z']|'_'
let chaine = (alpha)(alpha | chiffre)*
let maj = ['A'-'Z']
let min = ['a'-'z']
let term = (maj)(alpha | chiffre)*
let notTerm = (min)(alpha | chiffre)*
let chaineCode = [^'}']
let chaineType = [^'>']

rule token = parse 
	| chiffre(chiffre*) as i {INT (int_of_string i)}
	| "(" {LP}
	| ")" {RP}
	| "+" {ADD}
	| "*" {MUL}
	| " " {token lexbuf}
	| eof {EOF}
	
