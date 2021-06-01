(*
######################################
#                                    #
# Fichier definissant tous les types #
#       utilisés par Samenhir.       #
#    (inspiration venant du TD 6)    #
#                                    #
######################################
*)


(* Différentes structures utiles pour le calcul utilisé dans le TD 6 *)
type terminal = string
type non_terminal = string 

type symbol = 
	| Terminal of terminal
	| NonTerminal of non_terminal

type production = symbol list

type rule = non_terminal * production * (string option)

type grammar = {
	start : non_terminal;
	rules : rule list
}

module Ntset = Set.Make(String)
module Ntmap = Map.Make(String)

module Tset = Set.Make(String)
module Sset = Set.Make(String)

type nulls = Ntset.t
type first = Tset.t Ntmap.t 
type follow = Tset.t Ntmap.t

module Tmap = Map.Make(String)
module Pset = Set.Make(struct type t = production let compare = compare end)
module Rset = Set.Make(struct type t = rule let compare = compare end)
module Rmap = Map.Make(struct type t = rule let compare = compare end)

type expansion_table = Pset.t Tmap.t Ntmap.t

(* Redéfinis les types pour pouvoir stocker le point servant à se souvenir de la position dans la règle *)
type symbolD = 
	| TerminalD of terminal
	| NonTerminalD of non_terminal
	| Dot
type productionD = symbolD list
type ruleD = non_terminal * productionD * (string option)

module PDset = Set.Make(struct type t = productionD * (string option) let compare = compare end)
type strStr = non_terminal * terminal

module SSmap = Map.Make(struct type t = strStr let compare = compare end)

type grammarD = {
	startD : non_terminal;
	rulesD : ruleD list
}

(* Structures permettant de stocker l'automate non déterministe (ND) *)
type stateND = non_terminal * productionD * (string option) * terminal

module StateMap = Map.Make(struct type t = stateND let compare = compare end)
module Smap = Map.Make(String)
module StateSet = Set.Make(struct type t = stateND let compare = compare end)

module Imap = Map.Make(Int)
module Iset = Set.Make(Int)


type state = Iset.t
type transitionTableRaw = StateSet.t Smap.t StateMap.t
type transitionTable = state Smap.t Imap.t

type automatND = {
	startND : state;
	transND : transitionTable;
	conversionND: stateND Imap.t
}

(* Structures permettant de stocker l'automate déterministe (D) *)
module StateSetMap = Map.Make(Iset)
type transitionTableD = Iset.t Smap.t StateSetMap.t 
type successor = Iset.t Imap.t

type automatD = {
	startSet : state;
	transitions : transitionTableD;
	conversion : stateND Imap.t
}

(* Structures servant à stocker la table *)
type action = 
	| REDUCE of rule
	| SHIFT of int
	| SUCCESS


type assoc = 
	| Left
	| Right
	| NonAssoc

type priority = (assoc * non_terminal list) list
type actionTable = action Tmap.t Imap.t
type gotoTable = int Ntmap.t Imap.t

type parseTable = {
	startLine : int;
	goto : gotoTable;
	action : actionTable
}

(* Structures pour stocker la grammaire brute avec les opérations Ocaml à effectuer *)
type symbol_raw = 
	|TerminalR of terminal
	|AssocTerminal of (string * terminal)
	|NonTerminalR of non_terminal
	|AssocNonTerminal of (string * non_terminal)

type raw_production = symbol_raw list
type consequence = string
type raw_rule = (non_terminal * string * raw_production * (non_terminal option) * consequence)

type grammar_raw = {
	startR : non_terminal;
	raw_rules : raw_rule list
}

type parser = {
	header : string;
	g : grammar_raw;
	prio : priority;
	tokenList : (string * (string option)) list;
}

type program = {
	gR : grammar_raw;
	startLTable : int;
	gotoTab : gotoTable;
	actionTab : actionTable;
	tokenList : (string * (string option)) list;
	head : string;
};;
