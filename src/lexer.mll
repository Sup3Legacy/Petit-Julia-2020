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
  | nombre as i { Hyper.enableEnd (); INT (Hyper.position lexbuf,
      try Hyper.int_from_string i
      with _ -> raise (Lexing_error ("Overflowing integer : "^i))) }
  | nombre"(" as i {Hyper.enterPar ();Hyper.disableEnd ();
      let b =
      try Hyper.int_from_string (String.sub i 0 ((String.length i) - 1))
      with _ -> raise (Lexing_error ("Overflowing integer : "^i))
      in
      ENTIER_PARG (Hyper.position lexbuf, b)}
  | nombre ident as b {Hyper.enableEnd ();
      let (i, s) =
      try (Hyper.separate_int_ident b)
      with _ -> raise (Lexing_error ("Overflowing integer : "^b))
      in
      ENTIER_IDENT (Hyper.position lexbuf, i, s)
    }
  | ident"(" as s {Hyper.enterPar (); Hyper.disableEnd (); IDENT_PARG (Hyper.position lexbuf,(String.sub s 0 ((String.length s) - 1)))}
  | ")"ident as s {
    if Hyper.leavePar () then begin
      let p = Lexing.lexeme_start_p lexbuf in
      Printf.printf "File \"%s\", line %d, character %d :\n" !file p.pos_lnum p.pos_bol;
      Printf.printf "Syntax error : unoppened parenthesis\n";
    end;
    Hyper.enableEnd ();
    PARD_IDENT (Hyper.position lexbuf,(String.sub s 1 ((String.length s) - 1)))
  }
  | ident as s {
        let word = Hashtbl.find_opt Hyper.keywords s in
        match word with
        | Some token -> begin
          let _ = match token with 
            |TRUE _ | FALSE _| RETURN _| END _-> Hyper.enableEnd ()
            | ELSE _-> begin
              Hyper.disableEnd ();
              Hyper.dernierEstElse := true;
              end
            | IF _ -> if !Hyper.dernierEstElse then begin
              let p = Lexing.lexeme_start_p lexbuf in
              Printf.printf "File \"%s\", line %d, character %d-%d :\n" !file p.pos_lnum (p.pos_cnum - p.pos_bol) (p.pos_cnum - p.pos_bol+1);
              Printf.printf "Syntax error : else followed by if\n";
              exit 1
              end;
              Hyper.disableEnd ()
            | _ -> Hyper.disableEnd ()
          in Hyper.rajoutePosition token lexbuf
          end
        | None -> begin 
          Hyper.enableEnd ();
          IDENT (Hyper.position lexbuf,s)
          end
    }
  | "," {Hyper.disableEnd (); COMMA}
  | "+" {Hyper.disableEnd (); PLUS}
  | "-" {Hyper.disableEnd (); MINUS}
  | "=" {Hyper.disableEnd (); AFFECT}
  | "||" {Hyper.disableEnd (); OR}
  | "&&" {Hyper.disableEnd (); AND}
  | "==" {Hyper.disableEnd (); EQ}
  | "!=" {Hyper.disableEnd (); NEQ}
  | ">" {Hyper.disableEnd (); G}
  | "<" {Hyper.disableEnd (); L}
  | ">=" {Hyper.disableEnd (); GEQ}
  | "<=" {Hyper.disableEnd (); LEQ}
  | "*" {Hyper.disableEnd (); TIMES}
  | "^" {Hyper.disableEnd (); EXP}
  | "%" {Hyper.disableEnd (); MODULO}
  | "!" {Hyper.disableEnd (); NOT (Hyper.position lexbuf)}
  | "." {Hyper.disableEnd (); DOT}
  | "(" {Hyper.enterPar (); Hyper.disableEnd (); PARG (Hyper.position lexbuf)}
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
    Hyper.enableEnd ();
    PARD (Hyper.position lexbuf)
  }
  | "#" {comment lexbuf}
  | ":" {Hyper.disableEnd ();COLON}
  | ";" {Hyper.disableEnd ();SEMICOLON}
  | "::" {Hyper.disableEnd ();TYPE}
  | "\n" {
      new_line lexbuf;
      let b = !Hyper.canEnd in
      if b then Hyper.disableEnd ();
      if b then SEMICOLON
      else token lexbuf
    }
  | space {token lexbuf}
  | chaine as s {Hyper.enableEnd (); CHAINE  (Hyper.position lexbuf,(String.sub s 1 (String.length s - 2)))}
  | _ as c{
    let p = Lexing.lexeme_start_p lexbuf in
    Printf.printf "File \"%s\", line %d, character %d-%d :\n" !file p.pos_lnum (p.pos_cnum - p.pos_bol) (p.pos_cnum - p.pos_bol+1);
    if c = '"' then Printf.printf "Unclosed string\n"
    else Printf.printf "Syntax error : unkown char : \'%c\'\n" c;
    exit 1
  }
  | eof {EOF}

and comment = parse
  | "\n" {
    new_line lexbuf;
    let b = !Hyper.canEnd in
    if b then Hyper.disableEnd ();
    if b then SEMICOLON
    else token lexbuf
    }
  | _ {comment lexbuf}


{
  (*let () = token (Lexing.from_channel stdin) in*)
}
