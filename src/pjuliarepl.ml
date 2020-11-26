open Lexer
open Parser
open Ast
open Astype
open Typer
open Hyper
open Utilities
open Interp
open Logo

let continue = ref true;;
let instr = ref "";;
let logo_file = "logo";;

let gVenv = ref (Tmap.singleton "nothing" Nothing)
let gFenv = ref (Tmap.singleton "div" [[Int64; Int64], Int64])
let gSenv = ref (Tmap.empty : structEnv)
let gAenv = ref (Tmap.empty : argsEnv)

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

let flushed = ref false;;

let flush () =
  gVenv := Tmap.singleton "nothing" Nothing;
  gFenv := Tmap.singleton "div" [[Int64; Int64], Int64];
  gSenv := Tmap.empty;
  gAenv := Tmap.empty;
  flushed := true
;;

print_string logo2;;

while !continue do
  (* boucle principale *)
  print_newline();
  print_string "ρjυλια> ";
  instr := read_line();
  if startswith !instr "#exit" then exit 0;
  if startswith !instr "#flush" then flush ();
  try
    begin
      let lb =
      if startswith !instr "#run"
        then
          begin
            let n = String.length !instr in
            let file = open_in (String.sub !instr 5 (n - 5)) in
            file_name := String.sub !instr 5 (n - 5);
            Lexing.from_channel file
          end
        else
          begin
            file_name := "Console_input";
            Lexing.from_string !instr
          end
      in
      try
        begin
          let e = Parser.fichier Lexer.token lb in
          let () = Typer.verificationType e gVenv gFenv gSenv gAenv in
          (* print_endline (show_fichier e); *)
          interp_file e globVenv globFenv globSenv;
        end
      with a -> begin
        let b = Lexing.lexeme_start_p lb in
        let e = Lexing.lexeme_end_p lb in
        match a with
        | Lexer.Lexing_error s -> begin
            Printf.printf "File \"%s\", line %d, character %d-%d :\n" !(file_name) b.pos_lnum (b.pos_cnum - b.pos_bol) (e.pos_cnum - e.pos_bol);
              Printf.printf "Lexical error at lexeme : \"%s\"\n" s
            end
          | Parser.Error -> begin
              Printf.printf "File \"%s\", line %d, character %d-%d :\n" !(file_name) b.pos_lnum (b.pos_cnum - b.pos_bol) (e.pos_cnum - e.pos_bol);
              Printf.printf "Syntax error\n"
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
          | _ when !flushed -> Printf.printf "Flushed";
          | _ -> Printf.printf "Unkown error in file %s\n" !(file_name);
        end
    end
    with Sys_error s -> Printf.printf "%s" s;
  flushed := false;
done;
