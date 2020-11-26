open Lexer
open Parser
open Ast
open Astype
open Hyper
open Utilities
open Typer

let notAffiche = ref false;;
let parse_only = ref false;;
let type_only = ref false;;

let gVenv = ref (Tmap.singleton "nothing" Nothing)
let gFenv = ref (Tmap.singleton "div" [[Int64; Int64], Int64])
let gSenv = ref (Tmap.empty : structEnv)
let gAenv = ref (Tmap.empty : argsEnv)

let handle () =
  let c = open_in !(Hyper.file) in
  let lb = Lexing.from_channel c in
  try
    let e = Parser.fichier Lexer.token lb
    in
    if !parse_only then begin
      if !notAffiche then print_endline !(Hyper.file)
      else print_endline (show_fichier e); (* On peut switch entre afficher le nom (ie. compil réussie) et afficher l'arbre généré *)
      exit 0;
      end;
    let () = Typer.verificationType e gVenv gFenv gSenv gAenv in
    print_endline !(Hyper.file);
    exit 0;
  with a -> begin
      let b = Lexing.lexeme_start_p lb in
      let e = Lexing.lexeme_end_p lb in
      match a with
        | Lexer.Lexing_error s -> begin
            Printf.printf "File \"%s\", line %d, character %d-%d :\n" !(Hyper.file) b.pos_lnum (b.pos_cnum - b.pos_bol) (e.pos_cnum - e.pos_bol);
            Printf.printf "Lexical error at lexeme : \"%s\"\n" s
          end
        | Parser.Error -> begin
            Printf.printf "File \"%s\", line %d, character %d-%d :\n" !(Hyper.file) b.pos_lnum (b.pos_cnum - b.pos_bol) (e.pos_cnum - e.pos_bol);
            Printf.printf "Syntax error\n"
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
        | _ -> Printf.printf "Unkown error in file %s\n" !(Hyper.file);
      exit 1
    end
;;



let set_filename n =
  Hyper.file := n
;;


let main () =
  begin
    let speclist = [("-f", Arg.String(set_filename), "file to process.");
    ("-disable_print", Arg.Set notAffiche, "remove the print of the abstract.");
    ("--parse_only", Arg.Set parse_only, "Stop after parsing");
    ("--type_only", Arg.Set type_only, "Stop after typing");
    ] in
    Arg.parse speclist (fun x -> ()) "";
    handle ();
  end
;;

let () = main ();;
