
open SamenhirAst
open SamenhirLexer
open SamenhirParser
open Samenhir_utilities

let file = ref "";;


let main () =
	let spectlist = []
	in
	Arg.parse spectlist (fun f -> file := f) "";
	if !file = "" then 
		failwith "no file to compile"
	else if not (Filename.check_suffix !file ".txt")
		then failwith "not the good extension"
		else begin 
			let outfile = (Filename.chop_suffix !file ".txt" ^ ".ml") in
			let f = open_in !file in
			let buf = Lexing.from_channel f in
			let parsed = SamenhirParser.fichier SamenhirLexer.token buf in
			let () = close_in f in
			let table = buildTable (unrawGrammar parsed.g) parsed.prio in
			let p = {gR = parsed.g; startLTable = table.startLine; gotoTab = table.goto; actionTab = table.action; tokenList = parsed.tokenList; head = parsed.header} in
			let out = open_out outfile in
			let _ = pp_buildProg (Format.formatter_of_out_channel out) p in
			let () = close_out out in
			let out = open_out (outfile^"i") in
			let _ = pp_mli (Format.formatter_of_out_channel out) p in
			close_out out;
		end
;;

let () = main ();;
