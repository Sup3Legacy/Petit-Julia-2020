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
let neg = '-'nombre

let car = [' '-'!''#'-'['']'-'~'] | "\\n" | "\\t" | "\\\"" | "\\\\"
let chaine = '\"'(car*)'\"'
let notPar = [^'('-')']
let unclosedPar = '('(notPar*)eof
let space = (' ' | '\t')*

let docstring = "\"\"\""(car*)"\"\"\""

rule token = parse
  | docstring as s {Hyper.enableEnd (); DOCSTRING (String.sub s 3 (String.length s - 6))}
  | nombre as i { Hyper.enableEnd (); INT (Hyper.position lexbuf,
      try Hyper.int_from_string i
      with _ -> raise (Ast.Lexing_Error_Msg_Pos ("Overflowing integer : "^i, Hyper.position lexbuf))) }
  | nombre"(" as i {Hyper.enterPar ();Hyper.disableEnd ();
      let b =
      try Hyper.int_from_string (String.sub i 0 ((String.length i) - 1))
      with _ -> raise (Ast.Lexing_Error_Msg_Pos ("Overflowing integer : "^i, Hyper.position lexbuf))
      in
      ENTIER_PARG (Hyper.position lexbuf, b)}
  | nombre ident as b {Hyper.enableEnd ();
      let (i, s) =
      try (Hyper.separate_int_ident b)
      with _ -> raise (Ast.Lexing_Error_Msg_Pos ("Overflowing integer", Hyper.position lexbuf))
      in
      ENTIER_IDENT (Hyper.position lexbuf, i, s)
    }
  | ident"(" as s {Hyper.enterPar (); Hyper.disableEnd (); IDENT_PARG (Hyper.position lexbuf,(String.sub s 0 ((String.length s) - 1)))}
  | ")"ident as s {
    if Hyper.leavePar () then begin
      raise (Ast.Lexing_Error_Msg_Pos ("unoppened parenthesis", Hyper.position lexbuf))
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
              raise (Ast.Lexing_Error_Msg_Pos ("else followed by if", Hyper.position lexbuf))
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
  | "," {Hyper.disableEnd (); COMMA (Hyper.position lexbuf)}
  | "+" {Hyper.disableEnd (); PLUS (Hyper.position lexbuf)}
  | "-" {Hyper.disableEnd (); MINUS (Hyper.position lexbuf)}
  | "=" {Hyper.disableEnd (); AFFECT (Hyper.position lexbuf)}
  | "||" {Hyper.disableEnd (); OR (Hyper.position lexbuf)}
  | "&&" {Hyper.disableEnd (); AND (Hyper.position lexbuf)}
  | "==" {Hyper.disableEnd (); EQ (Hyper.position lexbuf)}
  | "!=" {Hyper.disableEnd (); NEQ (Hyper.position lexbuf)}
  | ">" {Hyper.disableEnd (); G (Hyper.position lexbuf)}
  | "<" {Hyper.disableEnd (); L (Hyper.position lexbuf)}
  | ">=" {Hyper.disableEnd (); GEQ (Hyper.position lexbuf)}
  | "<=" {Hyper.disableEnd (); LEQ (Hyper.position lexbuf)}
  | "*" {Hyper.disableEnd (); TIMES (Hyper.position lexbuf)}
  | "^" {Hyper.disableEnd (); EXP (Hyper.position lexbuf)}
  | "%" {Hyper.disableEnd (); MODULO (Hyper.position lexbuf)}
  | "!" {Hyper.disableEnd (); NOT (Hyper.position lexbuf)}
  | "." {Hyper.disableEnd (); DOT (Hyper.position lexbuf)}
  | "(" {Hyper.enterPar (); Hyper.disableEnd (); PARG (Hyper.position lexbuf)}
  | unclosedPar {
    raise (Ast.Lexing_Error_Msg_Pos ("unclosed parenthesis", Hyper.position lexbuf))
  }
  | ")" {
    if Hyper.leavePar () then begin
    raise (Ast.Lexing_Error_Msg_Pos ("unoppened parenthesis", Hyper.position lexbuf))
    end;
    Hyper.enableEnd ();
    PARD (Hyper.position lexbuf)
  }
  | "#" {comment lexbuf}
  | ":" {Hyper.disableEnd ();COLON (Hyper.position lexbuf)}
  | ";" {Hyper.disableEnd ();SEMICOLON (Hyper.position lexbuf)}
  | "::" {Hyper.disableEnd ();TYPE}
  | "\n" {
      new_line lexbuf;
      let b = !Hyper.canEnd in
      if b then Hyper.disableEnd ();
      if b then SEMICOLON (Hyper.position lexbuf)
      else token lexbuf
    }
  | space {token lexbuf}
  | chaine as s {Hyper.enableEnd (); CHAINE  (Hyper.position lexbuf,(String.sub s 1 (String.length s - 2)))}
  | _ as c{
    if c = '"' then raise (Ast.Lexing_Error_Msg_Pos ("unclosed string", Hyper.position lexbuf))
    else raise (Ast.Lexing_Error_Msg_Pos ("unknown char : " ^ (String.make 1 c), Hyper.position lexbuf))
  }
  | eof {EOF}

and comment = parse
  | "\n" {
    new_line lexbuf;
    let b = !Hyper.canEnd in
    if b then Hyper.disableEnd ();
    if b then SEMICOLON (Hyper.position lexbuf)
    else token lexbuf
    }
  | _ {comment lexbuf}


{
  (*let () = token (Lexing.from_channel stdin) in*)
}
