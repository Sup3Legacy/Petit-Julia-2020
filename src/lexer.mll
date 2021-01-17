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

  let string_buffer = Buffer.create 1024

}

let chiffre = ['0'-'9']
let lettre = ['a'-'z']|['A'-'Z']
let alpha = ['a'-'z']|['A'-'Z']|'_'

let ident_sub = (alpha)(alpha | chiffre)*
let ident = ident_sub(("::"ident_sub)*)
let nombre = chiffre+

let flottant = (chiffre+'.' | '.'chiffre+ | chiffre+'.'chiffre+)(('e'|'E')('-'|'+')?chiffre+)?
let car = [' '-'!''#'-'['']'-'~'] 
let notPar = [^'('-')']
let unclosedPar = '('(notPar*)eof
let space = (' ' | '\t')*

let docstring = "\"\"\""((car|'\n'|'\t')*)"\"\"\""

rule tokens = parse
  | docstring as s {Hyper.disableEnd (); [DOCSTRING (String.sub s 3 (String.length s - 6))]}
  | "@" {Hyper.disableEnd (); [CONCAT (Hyper.position lexbuf)]}
  | nombre as i { Hyper.enableEnd ();
    try [INT (Hyper.position lexbuf, Int64.of_string i)]
    with _ -> raise (Ast.Lexing_Error_Msg_Pos ("Overflowing integer", Hyper.position lexbuf)) }
  | flottant as f { Hyper.enableEnd ();
    try [FLOAT (Hyper.position lexbuf, float_of_string f)]
    with _ -> print_string "Ho ho ho"; raise (Ast.Lexing_Error_Msg_Pos ("Overflowing float", Hyper.position lexbuf)) }
  | "-"(flottant as f) { Hyper.enableEnd ();
    try [MINUS (Hyper.position lexbuf);FLOAT (Hyper.position lexbuf, float_of_string f)]
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
      try [ENTIER_IDENT (Hyper.position lexbuf, Int64.of_string nb, Hyper.treat_ident s)]
      with _ -> raise (Ast.Lexing_Error_Msg_Pos ("Overflowing integer", Hyper.position lexbuf))
    }
  | '-'(nombre as nb)(ident as s) {Hyper.enableEnd ();
      try [MINUS (Hyper.position lexbuf); ENTIER_IDENT (Hyper.position lexbuf,Int64.of_string nb, s)]
      with _ -> if nb = "9223372036854775808"
          then [ENTIER_IDENT (Hyper.position lexbuf, Int64.of_string ("-"^nb), s)]
          else raise (Ast.Lexing_Error_Msg_Pos ("Overflowing integer", Hyper.position lexbuf))
    }
  | (ident as s)"(" {Hyper.enterPar (); Hyper.disableEnd (); [IDENT_PARG (Hyper.position lexbuf, Hyper.treat_ident s)]}
  | ")" (ident as s) {
    if s = "true" || s = "false"
    then raise (Ast.Lexing_Error_Msg_Pos ("Illegal variable name "^s, Hyper.position lexbuf))
    else
      if Hyper.leavePar () then raise (Ast.Lexing_Error_Msg_Pos ("unoppened parenthesis", Hyper.position lexbuf));
    Hyper.enableEnd ();
    [PARD_IDENT (Hyper.position lexbuf, Hyper.treat_ident s)]
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
          [IDENT (Hyper.position lexbuf, Hyper.treat_ident s)]
          end
    }
  | "," {Hyper.disableEnd (); [COMMA (Hyper.position lexbuf)]}
  | "+" {Hyper.disableEnd (); [PLUS (Hyper.position lexbuf)]}
  | "+=" {Hyper.disableEnd (); [PLUS_AFFECT (Hyper.position lexbuf)]}
  | "*=" {Hyper.disableEnd (); [TIMES_AFFECT (Hyper.position lexbuf)]}
  | "-=" {Hyper.disableEnd (); [MINUS_AFFECT (Hyper.position lexbuf)]}
  | "|=" {Hyper.disableEnd (); [OR_AFFECT (Hyper.position lexbuf)]}
  | "&=" {Hyper.disableEnd (); [AND_AFFECT (Hyper.position lexbuf)]}
  | "/=" {Hyper.disableEnd (); [XOR_AFFECT (Hyper.position lexbuf)]}
  | "<<=" {Hyper.disableEnd (); [SHIFT_LEFT_AFFECT (Hyper.position lexbuf)]}
  | ">>=" {Hyper.disableEnd (); [SHIFT_RIGHT_AFFECT (Hyper.position lexbuf)]}
  | "-" {Hyper.disableEnd (); [MINUS (Hyper.position lexbuf)]}
  | "=" {Hyper.disableEnd (); [AFFECT (Hyper.position lexbuf)]}
  | "||" {Hyper.disableEnd (); [OR (Hyper.position lexbuf)]}
  | "&&" {Hyper.disableEnd (); [AND (Hyper.position lexbuf)]}
  | "==" {Hyper.disableEnd (); [EQ (Hyper.position lexbuf)]}
  | "!=" {Hyper.disableEnd (); [NEQ (Hyper.position lexbuf)]}
  | "|" {Hyper.disableEnd (); [BIT_OR (Hyper.position lexbuf)]}
  | "&" {Hyper.disableEnd (); [BIT_AND (Hyper.position lexbuf)]}
  | "/" {Hyper.disableEnd (); [BIT_XOR (Hyper.position lexbuf)]}
  | ">>" {Hyper.disableEnd (); [SHIFT_RIGHT (Hyper.position lexbuf)]}
  | "<<" {Hyper.disableEnd (); [SHIFT_LEFT (Hyper.position lexbuf)]}
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
  | "{" {Hyper.disableEnd (); [CURLYG (Hyper.position lexbuf)]}
  | "}" {Hyper.enableEnd (); [CURLYD (Hyper.position lexbuf)]}
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
  | '\''(_ as c)'\'' {Hyper.enableEnd ();
    [CHAR (Hyper.position lexbuf, Char.code c)]
  }
  | "\'\\n\'" {Hyper.enableEnd ();
    [CHAR (Hyper.position lexbuf, Char.code '\n')]
  }
  | "\'\\\\'" {Hyper.enableEnd ();
    [CHAR (Hyper.position lexbuf, Char.code '\\')]
  }
  | "\'\\t\'" {Hyper.enableEnd ();
    [CHAR (Hyper.position lexbuf, Char.code '\t')]
  }
  | "\'\\\"\'" {Hyper.enableEnd ();
    [CHAR (Hyper.position lexbuf, Char.code '\"')]
  }
  | "\'\\\'\'" {Hyper.enableEnd ();
    [CHAR (Hyper.position lexbuf, Char.code '\'')]
  }
  | space {tokens lexbuf}
  | '"'     { [CHAINE (Hyper.position lexbuf,string lexbuf)] }
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
  | eof {[EOF]}
  | _ {comment lexbuf}

and string = parse
  | '"'
      {Hyper.enableEnd (); let s = Buffer.contents string_buffer in
    Buffer.reset string_buffer;
    s }
  | "\\n"
      { Buffer.add_string string_buffer "\n";
    string lexbuf }
  | "\\\""
      { Buffer.add_string string_buffer "\"";
    string lexbuf }
  | "\\t"
      { Buffer.add_string string_buffer "\t";
    string lexbuf }
  | "\\\\"
      { Buffer.add_string string_buffer "\\";
    string lexbuf }
  | car as c
      { Buffer.add_char string_buffer c;
  string lexbuf }
  | _ as c {raise (Ast.Lexing_Error_Msg_Pos ("unknown char : " ^ (String.make 1 c), Hyper.position lexbuf))}
  | eof
      { raise (Lexing_error "unterminated string") }



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
