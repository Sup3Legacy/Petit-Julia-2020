open Ast
open Astype
(*open Lexer
open Parser*)
(*open Hyper
open Typer
open X86_64*)

let affiche = ref false;;
let parse_only = ref false;;
let type_only = ref false;;
let show_fName = ref false;;

let gVenv = ref (Tmap.singleton "nothing" (false,Nothing))
let gFenv = ref (Tmap.singleton "div" [[Int64; Int64], Int64])
let gSenv = ref (Tmap.empty : structEnv)
let gAenv = ref (Tmap.empty : argsEnv)

module Sset = Set.Make(String)
let afficheL l =
  let conversionTokenType s = match s with
    |"INT"|"CHAINE"|"IDENT"|"NOT"|"FALSE"|"TRUE"|"FOR"|"IF"|"WHILE"|"RETURN"
|"ENTIER_IDENT"|"IDENT_PARG"|"ENTIER_PARG"|"PARG_IDENT"|"separated_list_COMMA_expr"
|"expr_wMin_"|"expr_w_Ret"|"expr"|"whileExp"|"lvalue"|"lvalue_wMin_"|"bloc"
|"expr_bloc"|"bloc1" -> "value"
    |"PARG" -> "("
    |"PARD" -> ")"
    |"AFFECT" -> "="
    |"OR"|"AND"|"EQ"|"NEQ"|"L"|"G"|"LEQ"|"GEQ"|"PLUS"|"MINUS"|"TIMES"|"MODULO"|"EXP" -> "operator"
    |"DOT" -> "."
    |"ELSE"|"ELSEIF"|"else_exp" -> "else(if)"
    |"END" -> "end"
    |"FUNCTION"|"fonction"|"STRUCT"|"MUTABLE"|"DOCSTRING"|"fichier"|"declarations_list"|"structure"|"EOF" -> "declaration" 
    |"param_list"|"separated_list_COMMA_param"|"param" -> "parameter"
    |"typage" -> "type"
    |"TYPE" -> "::"
    |"COLON" -> ":"
    |"SEMICOLON"|"expr_bloc2"|"bloc_END"|"bloc1bis" -> ";"
    |"COMMA"|"separated_list_C_P"|"separated_list_C_E"-> ","
    |_ -> assert false
  in
  let rec aux (l:string list) = match l with
  |[] -> Sset.empty
  |hd::tl -> Sset.add (conversionTokenType hd) (aux tl)
  in Sset.iter (Printf.printf "%s ") (aux l);
  Printf.printf "\n"

let handle () =
  let c = open_in !(Hyper.file) in
  let lb = Lexing.from_channel c in
  try
    let e = Parser.fichier Lexer.token lb
    in
    if !parse_only then begin
      if !affiche then print_endline (show_fichier e)
      else if !show_fName then print_endline !(Hyper.file); (* On peut switch entre afficher le nom (ie. compil réussie) et afficher l'arbre généré *)
      exit 0;
      end;
    let () = Typer.typerCompilateur e gVenv gFenv gSenv gAenv in
    if !affiche then print_endline (show_fichier e)
    else if !show_fName then print_endline !(Hyper.file);
    exit 0;
  with a -> begin
      let b = Lexing.lexeme_start_p lb in
      let e = Lexing.lexeme_end_p lb in
      match a with
        | Lexer.Lexing_error s -> begin
            Printf.printf "File \"%s\", line %d, character %d-%d :\n" !(Hyper.file) b.pos_lnum (b.pos_cnum - b.pos_bol) (e.pos_cnum - e.pos_bol);
            Printf.printf "Lexical error at lexeme : \"%s\"\n" s
          end
        | Parser.Samenhir_Parsing_Error sl -> begin
            Printf.printf "File \"%s\", line %d, character %d-%d :\n" !(Hyper.file) b.pos_lnum (b.pos_cnum - b.pos_bol) (e.pos_cnum - e.pos_bol);
            Printf.printf "Syntax error, expected: ";afficheL sl
          end
        | Ast.Parsing_Error -> begin
            Printf.printf "File \"%s\", line %d, character %d-%d :\n" !(Hyper.file) b.pos_lnum (b.pos_cnum - b.pos_bol) (e.pos_cnum - e.pos_bol);
            Printf.printf "Syntax error\n"
          end
        | Ast.Typing_Error -> begin
            Printf.printf "File \"%s\", unknown position:\n" !(Hyper.file);
            Printf.printf "Typing error\n"
          end
        | Ast.Typing_Error_Msg m -> begin
            Printf.printf "File \"%s\", unknown position\n" !(Hyper.file);
            Printf.printf "Typing error : %s\n" m
          end
        | Ast.Typing_Error_Msg_Pos (m,p) -> begin
            Printf.printf "File \"%s\", line %d, character %d-%d :\n" !(Hyper.file) p.ldeb p.cdeb p.cfin;
            Printf.printf "Typing error : %s\n" m
          end
        | Ast.Lexing_Error -> begin
            Printf.printf "File \"%s\", unknown position:\n" !(Hyper.file);
            Printf.printf "Lexing error"
          end
        | Ast.Lexing_Error_Msg m -> begin
            Printf.printf "File \"%s\", unknown position:\n" !(Hyper.file);
            Printf.printf "Lexing error : %s\n" m
          end
        | Ast.Lexing_Error_Msg_Pos (m, p) -> begin
            Printf.printf "File \"%s\", line %d, character %d-%d :\n" !(Hyper.file) p.ldeb p.cdeb p.cfin;
            Printf.printf "Lexing error : %s\n" m
          end
        | _ -> Printf.printf "Unkown error in file %s\n" !(Hyper.file);
      exit 1
    end
;;



let set_filename n =
  Hyper.file := n
;;


let main () =
  begin
    let speclist = [
    ("-print_abstrac", Arg.Set affiche, "print of the abstract");
    ("--parse_only", Arg.Set parse_only, "Stop after parsing");
    ("--type_only", Arg.Set type_only, "Stop after typing");
    ("-show_file_name", Arg.Set show_fName, "Print the name of the file to compile")
    ] in
    Arg.parse speclist set_filename "file to process.";
    let file = open_out "out.s" in
    Printf.fprintf file "%s\n" "hahaha";
    close_out file;
    handle ();
  end
;;

let () = main ();;
