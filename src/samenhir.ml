(*
###########################################################
#                                                         #
#             Fichier d'interface de Samenhir             #
#                                                         #
# Sert à appeler le parser, le lexer, le constructeur de  #
# table ainsi que la fonction de construction des fichier #
#             .ml et .mli du même nom que l'input         #
#                                                         #
###########################################################
*)

open SamenhirAst

let file = ref "";;
let v2 = ref false;;
let language = ref "ocaml";;
let outfile = ref "";;

let lexer = ref "super::lexer";;

type lang =
	| OCaml
	| Rust

let convert_lang_name x = match String.lowercase_ascii x with 
	| "ocaml" | "ml" -> OCaml
	| "rust" | "rs" -> Rust
	| _ -> begin	
			print_string "This languga name is not autorised !\n";
			exit 1;
	end

let get_output_file_name fo fi l = 
	if fo <> "" then fo else 
	Filename.chop_suffix fi "sam" ^
	( match l with
			| OCaml -> "ml"
			| Rust -> "rs"
	)


let main () =
	let spectlist = ["-explain", Arg.Set Samenhir_utilities.explain, "build file of states";
	"-print", Arg.Set Samenhir_utilities.print_all, "show info to debug Samenhir";
	"-v2", Arg.Set v2, "use v2 version for OCaml only";
	"-l", Arg.Set_string language, "set language";
	"-include-main", Arg.Set Samenhir_utilities.include_main, "add main function for rust";
	"-lexer", Arg.Set_string lexer, "choose lexer name for rust";
	"-o", Arg.Set_string outfile, "select output name";
	"-trace-time", Arg.Set Samenhir_utilities.trace_time, "enable timer"]
	in
	Arg.parse spectlist (fun f -> file := f) "";
	if !file = "" then
		failwith "no file to compile"
	else if not (Filename.check_suffix !file ".sam")
		then failwith "not the good extension"
		else begin
			let f = open_in !file in
			let buf = Lexing.from_channel f in
			let t_start_parse = Unix.gettimeofday () in
			let parsed = try SamenhirParser.program SamenhirLexer.token buf
			with SamenhirParser.Samenhir_Parsing_Error _ -> (let e = Lexing.lexeme_end_p buf in
				print_string "Parsing error line ";
				print_int e.pos_lnum;
				print_newline ();
				exit 1)
 in
			let () = close_in f in
			let t_end_parse = Unix.gettimeofday () in
			if !Samenhir_utilities.trace_time then
				begin
					print_string "Parsed in ";
					print_float (t_end_parse -. t_start_parse);
					print_string "s";
					print_newline ()
				end;
			let t_start_build_table = Unix.gettimeofday () in
			let table = Samenhir_utilities.buildTable (Samenhir_utilities.unrawGrammar parsed.g) parsed.prio in
			let t_end_build_table = Unix.gettimeofday () in
			let p = {gR = parsed.g; startLTable = table.startLine; gotoTab = table.goto; actionTab = table.action; tokenList = parsed.tokenList; head = parsed.header} in
			if !Samenhir_utilities.trace_time then
				begin
					print_string "Build table in ";
					print_float (t_end_build_table -. t_start_build_table);
					print_string "s";
					print_newline ()
				end;
			outfile := get_output_file_name !outfile !file (convert_lang_name !language);
			let t_start_print_file = Unix.gettimeofday () in
			match convert_lang_name !language with 
				| OCaml -> begin
					if !Samenhir_utilities.include_main then print_string "WARNING : No main is not avialable for Ocaml\n";
					let out = open_out !outfile in
					let _ = (if !v2 then Samenhir_utilities.pp_main else Samenhir_utilities.pp_buildProg) (Format.formatter_of_out_channel out) p in
					let () = close_out out in
					let out = open_out (!outfile^"i") in
					let _ = Samenhir_utilities.pp_mli (Format.formatter_of_out_channel out) p in
					close_out out;
					end
				| Rust -> begin
					if !v2 then print_string "WARNING : V2 is directly implemented in rust, no need to call it\n";
					let out = open_out !outfile in
					let lexerName = "&mut "^(!lexer)^"::Lexbuf" in
					let _ = Samenhir_utilities.pp_rust_main (Format.formatter_of_out_channel out) p lexerName in
					close_out out
					end;
			let t_end_print_file = Unix.gettimeofday () in
			if !Samenhir_utilities.trace_time then
				begin
					print_string "Print file in ";
					print_float (t_end_print_file -. t_start_print_file);
					print_string "s";
					print_newline ()
				end;
		end
;;

let () = main ();;
