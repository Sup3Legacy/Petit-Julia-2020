type terminal = string
type non_terminal = string 

type symbol = 
	| Terminal of terminal
	| NonTerminal of non_terminal

type production = symbol list

type rule = non_terminal * production

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

type symbolD = 
	| TerminalD of terminal
	| NonTerminalD of non_terminal
	| Dot

type productionD = symbolD list
type ruleD = non_terminal * productionD

module PDset = Set.Make(struct type t = productionD let compare = compare end)
type strStr = non_terminal * terminal

module SSmap = Map.Make(struct type t = strStr let compare = compare end)

type grammarD = {
	startD : non_terminal;
	rulesD : ruleD list
}

type stateND = non_terminal * productionD * terminal

module StateMap = Map.Make(struct type t = stateND let compare = compare end)
module Smap = Map.Make(String)
module StateSet = Set.Make(struct type t = stateND let compare = compare end)

type state = StateSet.t
type transitionTable = state Smap.t StateMap.t
type prodSet = state Smap.t

type automatND = {
	startND : state;
	transND : transitionTable
}

module StateSetMap = Map.Make(StateSet)
type transitionTableD = state Smap.t StateSetMap.t 
type successor = state StateMap.t

type automatD = {
	startSet : state;
	transitions : transitionTableD
}


type assoc = 
	| Left
	| Right
	| NonAssoc

type action = 
	| REDUCE of rule
	| SHIFT of int
	| SUCCESS

module Imap = Map.Make(Int)

type priority = (assoc * non_terminal) list
type actionTable = action Tmap.t Imap.t
type gotoTable = int Ntmap.t Imap.t

type parseTables = {
	startLine : int;
	goto : gotoTable;
	action : actionTable
}

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

}
