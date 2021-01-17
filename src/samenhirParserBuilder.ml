(*
############################################
#                                          #
#  Fichier servant à construire le parser  #
# de samenhir. Il contient donc la syntaxe #
# 	   asbtraite du parser de Samenhir     #
#                                          #
############################################
*)

open SamenhirAst;;

let tokenList = [("CODE", Some "string");
	("HEADER", Some "string");
	("VERT", None);
	("TERM", Some "string");
	("N_TERM_IDENT", Some "string");
	("TYPE", Some "string");
	("ASSOC", None);
	("START", None);
	("SPLIT", None);
	("TOKEN", None);
	("POINTS", None);
	("SEMICOLON", None);
	("RIGHT", None);
	("LEFT", None);
	("NONASSOC", None);
	("PREC", None);
	("EOF", None)]
in let header = "open SamenhirAst\n" in
let startR = "program" in
let rawRules = [
	(*name, type, raw_prod, assoc, consequence *)
	("program", "SamenhirAst.parser",
		[AssocTerminal ("h","HEADER"); AssocNonTerminal ("tL","list_tokenDecl"); AssocNonTerminal ("aS", "list_assocDecl"); AssocNonTerminal ("s", "startDecl"); TerminalR "SPLIT"; AssocNonTerminal ("rL", "list_rulesDecl"); TerminalR "EOF"],
		None,
		"let rList = List.concat rL in let gramR = {startR = s; raw_rules = rList} in {header = h; g = gramR; prio = aS; tokenList = List.concat tL}\n"
	);
	("program", "SamenhirAst.parser",
		[AssocNonTerminal ("tL","list_tokenDecl"); AssocNonTerminal ("aS", "list_assocDecl"); AssocNonTerminal ("s", "startDecl"); TerminalR "SPLIT"; AssocNonTerminal ("rL", "list_rulesDecl"); TerminalR "EOF"],
		None,
		"let rList = List.concat rL in let gramR = {startR = s; raw_rules = rList} in {header = \"\"; g = gramR; prio = aS; tokenList = List.concat tL}\n"
	);
	
	("list_tokenDecl", "(string * (string option)) list list", [], None, "[]");
	("list_tokenDecl", "(string * (string option)) list list", [AssocNonTerminal ("t", "tokenDecl"); AssocNonTerminal ("tL", "list_tokenDecl")], None, "t::tL");
	
	("tokenDecl", "(string * (string option)) list", [TerminalR "TOKEN"; AssocTerminal ("t", "TYPE"); AssocTerminal ("term", "TERM")], None, "[term, Some t]");
	("tokenDecl", "(string * (string option)) list", [TerminalR "TOKEN"; AssocNonTerminal ("tL", "list_TERM")], None, "List.map (fun t -> (t, None)) tL");

	("list_TERM", "string list", [AssocTerminal ("t", "TERM")], None, "[t]");
	("list_TERM", "string list", [AssocTerminal ("t", "TERM"); AssocNonTerminal ("tL", "list_TERM")], None, "t::tL");

	("list_assocDecl", "SamenhirAst.priority", [], None, "[]");
	("list_assocDecl", "SamenhirAst.priority", [AssocNonTerminal ("a", "assocDecl"); AssocNonTerminal ("aL","list_assocDecl")], None, "a::aL");

	("assocDecl", "(SamenhirAst.assoc * string list)",[AssocNonTerminal ("a", "associativity"); AssocNonTerminal ("tL", "list_TERM")], None, "(a, tL)");
	("assocDecl", "(SamenhirAst.assoc * string list)",[AssocNonTerminal ("a", "associativity"); AssocTerminal ("t", "N_TERM_IDENT")], None, "(a, [t])");

	("associativity", "SamenhirAst.assoc", [TerminalR "LEFT"], None, "Left");
	("associativity", "SamenhirAst.assoc", [TerminalR "RIGHT"], None, "Right");
	("associativity", "SamenhirAst.assoc", [TerminalR "NONASSOC"], None, "NonAssoc");

	("startDecl", "string", [TerminalR "START"; AssocTerminal ("nt", "N_TERM_IDENT")], None, "nt");

	("list_rulesDecl", "SamenhirAst.raw_rule list list", [], None, "[]");
	("list_rulesDecl", "SamenhirAst.raw_rule list list", [AssocNonTerminal ("r", "rulesDecl"); AssocNonTerminal ("rL", "list_rulesDecl")], None, "r::rL");

	("rulesDecl", "SamenhirAst.raw_rule list", [AssocTerminal ("nt", "N_TERM_IDENT"); AssocTerminal ("t", "TYPE"); TerminalR "POINTS"; AssocNonTerminal ("rL", "list_rule_SEMICOLON")], None, "List.map (fun (tL, c, op) -> (nt, t, tL, op, c)) rL");

	("list_rule_SEMICOLON", "(SamenhirAst.raw_production * string * (string option)) list", [AssocNonTerminal ("r", "rule"); TerminalR "SEMICOLON"], None, "[r]");
	("list_rule_SEMICOLON", "(SamenhirAst.raw_production * string * (string option)) list", [AssocNonTerminal ("r", "rule"); AssocNonTerminal ("rL", "list_rule_SEMICOLON")], None, "r::rL");

	("rule", "(SamenhirAst.raw_production * string * (string option))", [TerminalR "VERT"; AssocNonTerminal ("tL", "list_ruleElement"); AssocTerminal ("c", "CODE")], None, "(tL, c, None)");
	("rule", "(SamenhirAst.raw_production * string * (string option))", [TerminalR "VERT"; AssocNonTerminal ("tL", "list_ruleElement"); TerminalR "PREC"; AssocTerminal ("nt", "N_TERM_IDENT"); AssocTerminal ("c", "CODE")], None, "(tL, c, Some nt)");

	("list_ruleElement", "SamenhirAst.raw_production", [], None , "[]");
	("list_ruleElement", "SamenhirAst.raw_production", [AssocNonTerminal ("r", "ruleElement"); AssocNonTerminal ("rL", "list_ruleElement")], None , "r::rL");

	("ruleElement", "SamenhirAst.symbol_raw", [AssocTerminal ("t", "TERM")], None, "TerminalR t");
	("ruleElement", "SamenhirAst.symbol_raw", [AssocTerminal ("t", "N_TERM_IDENT")], None, "NonTerminalR t");
	("ruleElement", "SamenhirAst.symbol_raw", [AssocTerminal ("t1", "N_TERM_IDENT"); TerminalR "ASSOC"; AssocTerminal ("t2", "TERM")], None, "AssocTerminal (t1,t2)");
	("ruleElement", "SamenhirAst.symbol_raw", [AssocTerminal ("t1", "N_TERM_IDENT"); TerminalR "ASSOC"; AssocTerminal ("t2", "N_TERM_IDENT")], None, "AssocNonTerminal (t1,t2)")

	] in
let g = {startR = startR; raw_rules = rawRules} in
let prio = [] in
let parsed = {header = header; g = g; prio = prio; tokenList = tokenList} in
let table = Samenhir_utilities.buildTable (Samenhir_utilities.unrawGrammar parsed.g) parsed.prio in
let p = {gR = parsed.g; startLTable = table.startLine; gotoTab = table.goto; actionTab = table.action; tokenList = parsed.tokenList; head = parsed.header} in
let out = open_out "samenhirParser.ml" in
let _ = Samenhir_utilities.pp_main (Format.formatter_of_out_channel out) p in
let () = close_out out in
let out = open_out "samenhirParser.mli" in
let _ = Samenhir_utilities.pp_mli (Format.formatter_of_out_channel out) p in
	close_out out
