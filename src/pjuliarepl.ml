(*
###########################################################
#                                                         #
#                           REPL                          #
#                                                         #
#   Il implémente la boucle de lecture écriture sur la    #
# sortie standard. Il gère aussi le chargement de fichier #
#   ainsi que l'appel aux fonctions de parsing et typage  #
#                                                         #
###########################################################
*)

open Ast
open Astype
open Astinterp

let continue = ref true;;
let instr = ref "";;
let prompt = ref "ρjυλια> ";;

(* Environnements globaux de typage *)
let gVenv = ref (Tmap.empty : Astype.varEnv)
let gFenv = ref (Tmap.empty : Astype.funcEnv)
let gSenv = ref (Tmap.empty : Astype.structEnv)
let gAenv = ref (Tmap.empty : Astype.argsEnv)
let () = Typer.resetVE gVenv
let () = Typer.resetFE gFenv
let () = Typer.resetSE gSenv
let () = Typer.resetAE gAenv

let file_name = ref "";;

let startswith str motif =
  let n = String.length str in
  let m = String.length motif in
  if m > n then false
  else
    begin
      let i = ref 0 in
      while !i < m && str.[!i] = motif.[!i] do
        i := !i + 1;
      done;
    !i = m
    end
;;

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
    |"OR"|"AND"|"EQ"|"NEQ"|"L"|"G"|"LEQ"|"GEQ"|"PLUS"|"MINUS"|"TIMES"|"MODULO"
    |"EXP" -> "operator"
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

let flushed = ref false;;

(* vidage des environnement de typage *)
let flush () =
  Typer.resetVE gVenv;
  Typer.resetFE gFenv;
  Typer.resetSE gSenv;
  Typer.resetAE gAenv;
  Interp.flush ();
  flushed := true
;;

print_string Logo.logo2;;

let read_input () =
  let res = ref "" in
  let last = ref (read_line ()) in
  while !last <> "" do
    res := !res ^ (!last ^ "\n");
    last := read_line ();
  done;
  !res
;;

while !continue do
  (* boucle principale *)
  flushed := false;
  print_newline();
  print_newline();
  print_string !prompt;
  instr := read_input();
  if startswith !instr "#exit" then exit 0;
  if startswith !instr "#flush" then flush ();
  if startswith !instr "#cof" then begin print_string "Choisissez la survivaliste"; instr := "" end;
    (* NDLR : un de nous se présente aux élections du COF sur la liste "suvivaliste" :D *)
  if startswith !instr "#tux" then begin print_string Logo.tux; instr := "" end;
  if startswith !instr "#update" then begin PPkg.update (); instr := "" end;
  if startswith !instr "#install" then
    begin let n = String.length !instr in
    let a = PPkg.download_package (String.sub !instr 9 (n - 10)) in a;
    instr := "" end;
  if startswith !instr "#remove" then 
    begin 
    let n = String.length !instr in
    PPkg.remove_package (String.sub !instr 8 (n - 9))
    end;
  if startswith !instr "? " then
    begin
      let n = String.length !instr in
      let identifiant = String.sub !instr 2 (n - 2) in
      Interp.retrieve_docstring identifiant;
      instr := "";
    end;
  try
    begin
      let lb =
        begin
          file_name := "Console_input";
          Lexing.from_string !instr
        end
      in
      try
        begin
          Parser.file_name := !file_name;
          let e = DepManager.get_parsed_file lb in
          let _ = Typer.typerRepl e gVenv gFenv gSenv gAenv in
          Interp.interp_file e;
        end
      with a -> begin
        let b = Lexing.lexeme_start_p lb in
        let e = Lexing.lexeme_end_p lb in
        match a with
          (* Récupération et affichage de erreurs *)
          | Lexer.Lexing_error s -> begin
            Printf.printf "File \"%s\", line %d, character %d-%d :\n" !(file_name) b.pos_lnum (b.pos_cnum - b.pos_bol) (e.pos_cnum - e.pos_bol);
              Printf.printf "Lexical error at lexeme : \"%s\"\n" s
            end
          | Parser.Samenhir_Parsing_Error sl -> begin
              Printf.printf "File \"%s\", line %d, character %d-%d :\n" !(file_name) b.pos_lnum (b.pos_cnum - b.pos_bol) (e.pos_cnum - e.pos_bol);
              if List.length sl < 0 then (Printf.printf "Syntax error, expected: ";afficheL sl)
              else Printf.printf "Syntax error\n"
            end
          | Ast.Parsing_Error -> begin
              Printf.printf "File \"%s\", line %d, character %d-%d :\n" !(file_name) b.pos_lnum (b.pos_cnum - b.pos_bol) (e.pos_cnum - e.pos_bol);
              Printf.printf "Syntax error\n"
            end
          | Ast.Typing_Error -> begin
              Printf.printf "File \"%s\", unknown position:\n" !(file_name);
              Printf.printf "Typing error\n"
            end
          | Ast.Typing_Error_Msg m -> begin
              Printf.printf "File \"%s\", unknown position\n" !(file_name);
              Printf.printf "Typing error : %s\n" m
            end
          | Ast.Typing_Error_Msg_Pos (m,p) -> begin
              Printf.printf "File \"%s\", line %d, character %d-%d :\n" !(file_name) p.ldeb p.cdeb p.cfin;
              Printf.printf "Typing error : %s\n" m
            end
          | Ast.Interp_Error_Msg m -> begin
              Printf.printf "File \"%s\", unknown position:\n" !(file_name);
              Printf.printf "Interpretation error : %s\n" m
            end
          | Ast.Lexing_Error -> begin
              Printf.printf "File \"%s\", unknown position:\n" !(file_name);
              Printf.printf "Lexing error"
            end
          | Ast.Lexing_Error_Msg m -> begin
              Printf.printf "File \"%s\", unknown position:\n" !(file_name);
              Printf.printf "Lexing error : %s\n" m
            end
          | Ast.Lexing_Error_Msg_Pos (m, p) -> begin
              Printf.printf "File \"%s\", line %d, character %d-%d :\n" !(file_name) p.ldeb p.cdeb p.cfin;
              Printf.printf "Lexing error : %s\n" m
            end
          | _ when !flushed -> Printf.printf "Flushed";
          | a -> raise a
        end
    end
    with Sys_error s -> Printf.printf "%s" s;
done;
