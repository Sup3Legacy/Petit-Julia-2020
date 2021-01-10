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


let main () =
	let spectlist = ["-explain", Arg.Set Samenhir_utilities.explain, "build file of states";
	"-print", Arg.Set Samenhir_utilities.print_all, "show info to debug Samenhir"]
	in
	Arg.parse spectlist (fun f -> file := f) "";
	if !file = "" then
		failwith "no file to compile"
	else if not (Filename.check_suffix !file ".sam")
		then failwith "not the good extension"
		else begin
			let outfile = (Filename.chop_suffix !file ".sam" ^ ".ml") in
			let f = open_in !file in
			let buf = Lexing.from_channel f in
			let parsed = try SamenhirParser.program SamenhirLexer.token buf
			with SamenhirParser.Samenhir_Parsing_Error _ -> (let e = Lexing.lexeme_end_p buf in
				print_int e.pos_lnum;
				print_newline ();
				exit 1)
 in
			let () = close_in f in
			let table = Samenhir_utilities.buildTable (Samenhir_utilities.unrawGrammar parsed.g) parsed.prio in
			let p = {gR = parsed.g; startLTable = table.startLine; gotoTab = table.goto; actionTab = table.action; tokenList = parsed.tokenList; head = parsed.header} in
			let out = open_out outfile in
			let _ = Samenhir_utilities.pp_buildProg (Format.formatter_of_out_channel out) p in
			let () = close_out out in
			let out = open_out (outfile^"i") in
			let _ = Samenhir_utilities.pp_mli (Format.formatter_of_out_channel out) p in
			close_out out;
		end
;;

let () = main ();;
