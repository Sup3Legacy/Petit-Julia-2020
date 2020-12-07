(*
###########################################################
#                                                         #
#             Déclaration des règles du lexer             #
#                                                         #
###########################################################
*)

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

let flottant = (chiffre+'.' | '.'chiffre+ | chiffre+'.'chiffre+)(('e'|'E')('-'|'+')?chiffre+)?
let car = [' '-'!''#'-'['']'-'~'] | "\\n" | "\\t" | "\\\"" | "\\\\"
let chaine = '\"'(car*)'\"'
let notPar = [^'('-')']
let unclosedPar = '('(notPar*)eof
let space = (' ' | '\t')*

let docstring = "\"\"\""((car|'\n'|'\t')*)"\"\"\""

rule tokens = parse
  | docstring as s {Hyper.disableEnd (); [DOCSTRING (String.sub s 3 (String.length s - 6))]}
  | nombre as i { Hyper.enableEnd ();
    try [INT (Hyper.position lexbuf, Int64.of_string i)]
    with _ -> raise (Ast.Lexing_Error_Msg_Pos ("Overflowing integer", Hyper.position lexbuf)) }
  | flottant as f { Hyper.enableEnd ();
    try [FLOAT (Hyper.position lexbuf, float_of_string f)]
    with _ -> print_string "Ho ho ho"; raise (Ast.Lexing_Error_Msg_Pos ("Overflowing float", Hyper.position lexbuf)) }
  | "-"(nombre as i) { Hyper.enableEnd ();
    try [MINUS (Hyper.position lexbuf);INT (Hyper.position lexbuf, Int64.of_string i)]
    with _ -> if i = "9223372036854775808"
          then [INT (Hyper.position lexbuf, Int64.of_string ("-"^i))]
          else raise (Ast.Lexing_Error_Msg_Pos ("Overflowing integer", Hyper.position lexbuf))}
  | (nombre as i)"(" {Hyper.enterPar ();Hyper.disableEnd ();
      let i = try Int64.of_string i with _ -> raise (Ast.Lexing_Error_Msg_Pos ("Overflowing integer", Hyper.position lexbuf)) in
      [ENTIER_PARG (Hyper.position lexbuf, i)]}
  | '-'(nombre as i)'(' {Hyper.enterPar (); Hyper.disableEnd ();
      try [MINUS (Hyper.position lexbuf); ENTIER_PARG (Hyper.position lexbuf, Int64.of_string i)]
      with _ -> if i = "9223372036854775808"
          then [ENTIER_PARG (Hyper.position lexbuf, Int64.of_string ("-"^i))]
          else raise (Ast.Lexing_Error_Msg_Pos ("Overflowing integer", Hyper.position lexbuf))
      }
  | (nombre as nb)(ident as s) {Hyper.enableEnd ();
      try [ENTIER_IDENT (Hyper.position lexbuf, Int64.of_string nb, s)]
      with _ -> raise (Ast.Lexing_Error_Msg_Pos ("Overflowing integer", Hyper.position lexbuf))
    }
  | '-'(nombre as nb)(ident as s) {Hyper.enableEnd ();
      try [MINUS (Hyper.position lexbuf); ENTIER_IDENT (Hyper.position lexbuf,Int64.of_string nb, s)]
      with _ -> if nb = "9223372036854775808"
          then [ENTIER_IDENT (Hyper.position lexbuf, Int64.of_string ("-"^nb), s)]
          else raise (Ast.Lexing_Error_Msg_Pos ("Overflowing integer", Hyper.position lexbuf))
    }
  | (ident as s)"(" {Hyper.enterPar (); Hyper.disableEnd (); [IDENT_PARG (Hyper.position lexbuf,s)]}
  | ")" (ident as s) {
    if s = "true" || s = "false"
    then raise (Ast.Lexing_Error_Msg_Pos ("Illegal variable name "^s, Hyper.position lexbuf))
    else
      if Hyper.leavePar () then raise (Ast.Lexing_Error_Msg_Pos ("unoppened parenthesis", Hyper.position lexbuf));
    Hyper.enableEnd ();
    [PARD_IDENT (Hyper.position lexbuf,s)]
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
          in [Hyper.rajoutePosition token lexbuf]
          end
        | None -> begin
          Hyper.enableEnd ();
          [IDENT (Hyper.position lexbuf,s)]
          end
    }
  | "," {Hyper.disableEnd (); [COMMA (Hyper.position lexbuf)]}
  | "+" {Hyper.disableEnd (); [PLUS (Hyper.position lexbuf)]}
  | "-" {Hyper.disableEnd (); [MINUS (Hyper.position lexbuf)]}
  | "=" {Hyper.disableEnd (); [AFFECT (Hyper.position lexbuf)]}
  | "||" {Hyper.disableEnd (); [OR (Hyper.position lexbuf)]}
  | "&&" {Hyper.disableEnd (); [AND (Hyper.position lexbuf)]}
  | "==" {Hyper.disableEnd (); [EQ (Hyper.position lexbuf)]}
  | "!=" {Hyper.disableEnd (); [NEQ (Hyper.position lexbuf)]}
  | ">" {Hyper.disableEnd (); [G (Hyper.position lexbuf)]}
  | "<" {Hyper.disableEnd (); [L (Hyper.position lexbuf)]}
  | ">=" {Hyper.disableEnd (); [GEQ (Hyper.position lexbuf)]}
  | "<=" {Hyper.disableEnd (); [LEQ (Hyper.position lexbuf)]}
  | "*" {Hyper.disableEnd (); [TIMES (Hyper.position lexbuf)]}
  | "^" {Hyper.disableEnd (); [EXP (Hyper.position lexbuf)]}
  | "%" {Hyper.disableEnd (); [MODULO (Hyper.position lexbuf)]}
  | "!" {Hyper.disableEnd (); [NOT (Hyper.position lexbuf)]}
  | "." {Hyper.disableEnd (); [DOT (Hyper.position lexbuf)]}
  | "(" {Hyper.enterPar (); Hyper.disableEnd (); [PARG (Hyper.position lexbuf)]}
  | unclosedPar {
    raise (Ast.Lexing_Error_Msg_Pos ("unclosed parenthesis", Hyper.position lexbuf))
  }
  | ")" {
    if Hyper.leavePar () then begin
    raise (Ast.Lexing_Error_Msg_Pos ("unoppened parenthesis", Hyper.position lexbuf))
    end;
    Hyper.enableEnd ();
    [PARD (Hyper.position lexbuf)]
  }
  | "[" {Hyper.disableEnd (); [CROCHETG (Hyper.position lexbuf)]}
  | "]" {Hyper.enableEnd (); [CROCHETD (Hyper.position lexbuf)]}
  | "#" {comment lexbuf}
  | ":" {Hyper.disableEnd ();[COLON (Hyper.position lexbuf)]}
  | ";" {Hyper.disableEnd ();[SEMICOLON (Hyper.position lexbuf)]}
  | "::" {Hyper.disableEnd ();[TYPE]}
  | "\n" {
      new_line lexbuf;
      let b = !Hyper.canEnd in
      if b then Hyper.disableEnd ();
      if b then [SEMICOLON (Hyper.position lexbuf)]
      else tokens lexbuf
    }
  | space {tokens lexbuf}
  | chaine as s {Hyper.enableEnd (); [CHAINE  (Hyper.position lexbuf,(String.sub s 1 (String.length s - 2)))]}
  | _ as c{
    if c = '"' then raise (Ast.Lexing_Error_Msg_Pos ("unclosed string", Hyper.position lexbuf))
    else raise (Ast.Lexing_Error_Msg_Pos ("unknown char : " ^ (String.make 1 c), Hyper.position lexbuf))
  }
  | eof {[EOF]}

and comment = parse
  | "\n" {
    new_line lexbuf;
    let b = !Hyper.canEnd in
    if b then Hyper.disableEnd ();
    if b then [SEMICOLON (Hyper.position lexbuf)]
    else tokens lexbuf
    }
  | _ {comment lexbuf}


{
    let token =
    let tokenQ = Queue.create () in
    (fun lb ->
      (if Queue.is_empty tokenQ then begin
  let l = tokens lb in
  List.iter (fun t -> Queue.add t tokenQ) l
  end;
  Queue.pop tokenQ))
}
