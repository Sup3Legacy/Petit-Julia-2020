{
  open Lexing
  open Parser
  open Hyper
  open Ast

  exception Lexing_error of string

  let file = ref "test.jl";;

}

let chiffre = ['0'-'9']
let alpha = ['a'-'z']|['A'-'Z']|'_'

let ident = (alpha)(alpha | chiffre)*
let nombre = chiffre+
let neg = '-'nombre

let car = [' '-'!''#'-'['']'-'~'] | "\\n" | "\\t" | "\\\"" | "\\\\"
let chaine = '\"'(car*)'\"'
let notPar = [^'('-')']
let unclosedPar = '('(notPar*)eof
let space = (' ' | '\t')*

rule token = parse
  | nombre as i { Hyper.enableEnd lexbuf; INT (
      try Hyper.int_from_string i
      with _ -> raise (Lexing_error ("Overflowing integer : "^i))) }
  | nombre"(" as i {Hyper.enterPar ();Hyper.disableEnd lexbuf;
      let b =
      try Hyper.int_from_string (String.sub i 0 ((String.length i) - 1))
      with _ -> raise (Lexing_error ("Overflowing integer : "^i))
      in
      ENTIER_PARG b}
  | nombre ident as b {Hyper.enableEnd lexbuf;
      let (i, s) =
      try (Hyper.separate_int_ident b)
      with _ -> raise (Lexing_error ("Overflowing integer : "^b))
      in
      ENTIER_IDENT (i, s)
    }
  | ident"(" as s {Hyper.enterPar (); Hyper.disableEnd lexbuf; IDENT_PARG (String.sub s 0 ((String.length s) - 1))}
  | ")"ident as s {
    if Hyper.leavePar () then begin
      let p = Lexing.lexeme_start_p lexbuf in
      Printf.printf "File \"%s\", line %d, character %d :\n" !file p.pos_lnum p.pos_bol;
      Printf.printf "Syntax error : unoppened parenthesis\n";
    end;
    Hyper.enableEnd lexbuf;
    PARD_IDENT (String.sub s 1 ((String.length s) - 1))
  }
  | ident as s {
        let word = Hashtbl.find_opt Hyper.keywords s in
        match word with
        | Some token -> begin
          let _ = match token with 
            |TRUE | FALSE | RETURN | END -> Hyper.enableEnd lexbuf
            | ELSE -> begin
              Hyper.disableEnd lexbuf;
              Hyper.dernierEstElse := true;
              end
            | IF -> if !Hyper.dernierEstElse then begin
              let p = Lexing.lexeme_start_p lexbuf in
              Printf.printf "File \"%s\", line %d, character %d-%d :\n" !file p.pos_lnum (p.pos_cnum - p.pos_bol) (p.pos_cnum - p.pos_bol+1);
              Printf.printf "Syntax error : else followed by if\n";
              exit 1
              end;
              Hyper.disableEnd lexbuf
            | _ -> Hyper.disableEnd lexbuf
          in token
          end
        | None -> begin 
          Hyper.enableEnd lexbuf;
          IDENT s
          end
    }
  | "," {Hyper.disableEnd lexbuf; COMMA}
  | "+" {Hyper.disableEnd lexbuf; PLUS}
  | "-" {Hyper.disableEnd lexbuf; MINUS}
  | "=" {Hyper.disableEnd lexbuf; AFFECT}
  | "||" {Hyper.disableEnd lexbuf; OR}
  | "&&" {Hyper.disableEnd lexbuf; AND}
  | "==" {Hyper.disableEnd lexbuf;EQ}
  | "!=" {Hyper.disableEnd lexbuf; NEQ}
  | ">" {Hyper.disableEnd lexbuf;G}
  | "<" {Hyper.disableEnd lexbuf;L}
  | ">=" {Hyper.disableEnd lexbuf;GEQ}
  | "<=" {Hyper.disableEnd lexbuf;LEQ}
  | "*" {Hyper.disableEnd lexbuf;TIMES}
  | "^" {Hyper.disableEnd lexbuf;EXP}
  | "%" {Hyper.disableEnd lexbuf;MODULO}
  | "!" {Hyper.disableEnd lexbuf;NOT}
  | "." {Hyper.disableEnd lexbuf;DOT}
  | "(" {Hyper.enterPar (); Hyper.disableEnd lexbuf; PARG}
  | unclosedPar {
    let p = Lexing.lexeme_start_p lexbuf in
    Printf.printf "File \"%s\", line %d, character %d-%d :\n" !file p.pos_lnum (p.pos_cnum - p.pos_bol) (p.pos_cnum - p.pos_bol+1);
    Printf.printf "Syntax error : unclosed parenthesis\n";
    exit 1
  }
  | ")" {
    if Hyper.leavePar () then begin
    let p = Lexing.lexeme_start_p lexbuf in
    Printf.printf "File \"%s\", line %d, character %d-%d :\n" !file p.pos_lnum (p.pos_cnum - p.pos_bol) (p.pos_cnum - p.pos_bol+1);
    Printf.printf "Syntax error : unoppened parenthesis\n";
    end;
    Hyper.enableEnd lexbuf;
    PARD
  }
  | "#" {comment lexbuf}
  | ":" {Hyper.disableEnd lexbuf;COLON}
  | ";" {Hyper.disableEnd lexbuf;SEMICOLON}
  | "::" {Hyper.disableEnd lexbuf;TYPE}
  | "\n" {
      new_line lexbuf;
      let b = !Hyper.canEnd in
      if b then Hyper.disableEnd lexbuf;
      if b then SEMICOLON
      else token lexbuf
    }
  | space {token lexbuf}
  | chaine as s {Hyper.enableEnd lexbuf; CHAINE (String.sub s 1 (String.length s - 2))}
  | _ as c{
    let p = Lexing.lexeme_start_p lexbuf in
    Printf.printf "File \"%s\", line %d, character %d-%d :\n" !file p.pos_lnum (p.pos_cnum - p.pos_bol) (p.pos_cnum - p.pos_bol+1);
    Printf.printf "Syntax error : unkown char : \'%c\'\n" c;
    exit 1
  }
  | eof {EOF}

and comment = parse
  | "\n" {
    new_line lexbuf;
    let b = !Hyper.canEnd in
    if b then Hyper.disableEnd lexbuf;
    if b then SEMICOLON
    else token lexbuf
    }
  | _ {comment lexbuf}


{
  (*let () = token (Lexing.from_channel stdin) in*)
}
