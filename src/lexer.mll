{
  open Lexing
  open Parser
  open Hyper
  open Ast

  exception Lexing_error of string
}

let chiffre = ['0'-'9']
let alpha = ['a'-'z']|['A'-'Z']|'_'

let ident = (alpha)(alpha | chiffre)*
let nombre = chiffre+

let car = [' '-'~'] | '\\' | '\"' | '\n' | '\t'
let chaine = '\"'(car*)'\"'

let space = (' ' | '\t')*

rule token = parse
  | nombre as i { Hyper.enableEnd (); INT (int_of_string i) }
  | nombre"(" as i {Hyper.disableEnd ();ENTIER_PARG (int_of_string (String.sub i 0 ((String.length i) - 1)))}
  | nombre ident as b {Hyper.disableEnd ();
      let (i, s) = Hyper.separate_int_ident b in
      ENTIER_IDENT (i, s)
    }
  | ident"(" as s {Hyper.disableEnd (); IDENT_PARG (String.sub s 0 ((String.length s) - 1))}
  | ")"ident as s {Hyper.enableEnd (); PARD_IDENT (String.sub s 1 ((String.length s) - 1))}
  | ident as s { Hyper.enableEnd ();
      let word = Hashtbl.find_opt Hyper.keywords s in
        match word with
        | Some token -> token
        | None -> IDENT s
    }
  | "," {Hyper.disableEnd (); COMMA}
  | "+" {Hyper.disableEnd (); PLUS}
  | "-" {Hyper.disableEnd (); MINUS}
  | "=" {Hyper.disableEnd (); AFFECT}
  | "||" {Hyper.disableEnd (); OR}
  | "&&" {Hyper.disableEnd (); AND}
  | "==" {Hyper.disableEnd ();EQ}
  | "!=" {Hyper.disableEnd (); NEQ}
  | ">" {Hyper.disableEnd ();G}
  | "<" {Hyper.disableEnd ();L}
  | ">=" {Hyper.disableEnd ();GEQ}
  | "<=" {Hyper.disableEnd ();LEQ}
  | "*" {Hyper.disableEnd ();TIMES}
  | "^" {Hyper.disableEnd ();EXP}
  | "%" {Hyper.disableEnd ();MODULO}
  | "!" {Hyper.disableEnd ();NOT}
  | "." {Hyper.disableEnd ();DOT}
  | "(" {Hyper.disableEnd ();PARG}
  | ")" {Hyper.enableEnd ();PARD}
  | "#" {comment lexbuf}
  | ":" {Hyper.disableEnd ();COLON}
  | ";" {Hyper.disableEnd (); SEMICOLON}
  | "::" {Hyper.disableEnd ();TYPE}
  | "\n" {
      new_line lexbuf;
      let b = !Hyper.canEnd in
      (* Hyper.disableEnd (); ATTENTION cela ferait planter si on a plusieurs sauts consécitufs de ligne, je pense *)
      if b then SEMICOLON
      else token lexbuf
    }
  | space {token lexbuf}
  | '"' {string_handle lexbuf}
  | _ as c {raise (Lexing_error ("Invalid character : '"^(String.make 1 c)^"'"))}
  | eof {EOF}

and comment = parse
  | chaine as s {comment lexbuf}
  | "\\n" {comment lexbuf}
  | "\n" {
      new_line lexbuf;
      (* Hyper.disableEnd (); ATTENTION cela ferait planter si on a plusieurs sauts consécitufs de ligne, je pense *)
      token lexbuf
    }
  | _ {comment lexbuf}

and string_handle = parse
  | '"' {CHAINE (empty_stack ())}
  | _ as c {push_to_string_stack c; string_handle lexbuf}
  | eof {raise (Lexing_error "Unfinished string")}

{
  (*let () = token (Lexing.from_channel stdin) in*)
}
