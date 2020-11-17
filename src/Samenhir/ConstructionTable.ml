
let print_all = false

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
type raw_rules = (non_terminal * raw_production * consequence)
type grammar_raw = {
	startR : non_terminal;
	raw_rules : raw_rules
}

type parser = {
	g : grammar_raw;
	prio : priority
}

let g_arith =
  { start = "S'";
    rules = [ "S'", [ NonTerminal "E"; Terminal "#" ];
              "E",  [ NonTerminal "T"; NonTerminal "E'"; ];
              "E'", [ Terminal "+"; NonTerminal "T"; NonTerminal "E'"; ];
              "E'", [ ];
              "T",  [ NonTerminal "F"; NonTerminal "T'"; ];
              "T'", [ Terminal "*"; NonTerminal "F"; NonTerminal "T'"; ];
              "T'", [ ];
              "F",  [ Terminal "("; NonTerminal "E"; Terminal ")"; ];
              "F",  [ Terminal "int" ]; ] }

let g1 = {
  start = "S'";
  rules = ["S'", [NonTerminal "S"; Terminal "#"];
	   "S", [];
	   "S", [Terminal "a"; NonTerminal "A"; NonTerminal "S"];
	   "S", [Terminal "b"; NonTerminal "B"; NonTerminal "S"];
	   "A", [Terminal "a"; NonTerminal "A"; NonTerminal "A"];
	   "A", [Terminal "b"];
	   "B", [Terminal "b"; NonTerminal "B"; NonTerminal "B"];
	   "B", [Terminal "a"];
	  ] }

let g_gram =
  { start = "S'";
    rules = [ "S'", [ NonTerminal "S"; Terminal "#" ];
              "S",  [ NonTerminal "R" ];
              "S",  [ NonTerminal "R"; Terminal ";"; NonTerminal "S" ];
              "R",  [ Terminal "ident"; Terminal "::="; NonTerminal "P"];
              "P",  [ NonTerminal "W" ];
              "P",  [ NonTerminal "W"; Terminal "|"; NonTerminal "P" ];
              "W",  [ ];
              "W",  [ NonTerminal "C"; NonTerminal "W";];
              "C",  [ Terminal "ident"];
              "C",  [ Terminal "string"];
            ] };;

let g_cours = 
	{ 	start = "S";
		rules = [ "S", [NonTerminal "E"; Terminal "#"];
				"E", [NonTerminal "E"; Terminal "+"; NonTerminal "T"];
				"E", [NonTerminal "T"];
				"T", [NonTerminal "T"; Terminal "*"; NonTerminal "F"];
				"T", [NonTerminal "F"];
				"F", [Terminal "("; NonTerminal "E"; Terminal ")"];
				"F", [Terminal "int"]
			] };;

let fixpoint f x =
	let t = ref true in 
	let x2 = ref x in 
	while !t do
		let (x3,t2) = f !x2 in 
		x2 := x3;
		t := t2;
	done;
	!x2;;

let is_null_production (valNulles:nulls) (prod:production) = 
	let rec aux = function
		| [] -> true
		| hd::tl -> begin
			match hd with
			| NonTerminal _ -> false
			| Terminal t -> (Ntset.mem t valNulles)&&(aux tl)
		end
	in aux prod

let null g :nulls = 
	let step nulls = 
		let nulSet = ref nulls in 
		let t = ref false in
		let aux (nom,prod) = 
			if Ntset.mem nom nulls then () else begin
				if is_null_production nulls prod then begin
						nulSet := Ntset.add nom !nulSet;
						t := true
				end
			end
		in List.iter aux g.rules;
		(!nulSet, !t)
	in fixpoint step Ntset.empty

let empty_map g = 
	let dico = ref Ntmap.empty in 
	List.iter (fun (n,p) -> dico := Ntmap.add n Tset.empty !dico) g.rules;
	!dico

let empty_mapS g = failwith "Not Implemented empty_mapS"

let first_production_step (valNulles:nulls) (fst:first) (prod:production) = 
	let rec aux = function
		|[] -> Tset.empty
		|(Terminal t)::tl -> Tset.singleton t 
		|(NonTerminal t)::tl -> if Ntset.mem t valNulles
			then Tset.add t (aux tl)
			else Ntmap.find t fst
	in aux prod

let first g (nul:nulls) :first = 
	let empty = empty_map g in 
	let step fst = 
		let aux fst2 (nom,prod) = 
			Ntmap.add nom (Tset.union (Ntmap.find nom fst2) (first_production_step nul fst prod)) fst2
		in let fstMap = List.fold_left aux empty g.rules in
		(fstMap, not (Ntmap.equal Tset.subset fstMap fst)) in
	fixpoint step empty

let follow g (n:nulls) (fst:first) :follow = 
	let update (follows,b) nt follow_nt= 
		let old = Ntmap.find nt follows in 
		(Ntmap.add nt (Tset.union follow_nt (Ntmap.find nt follows)) follows, b || not (Tset.subset follow_nt old))
	in let rec estNul = function
		|[] -> true
		|(Terminal _)::tl -> false
		|(NonTerminal t)::tl -> (Ntset.mem t n) && estNul tl
	in let rec fstf = function
		|[] -> Tset.empty
		|(Terminal t)::tl -> Tset.singleton t
		|(NonTerminal t)::tl -> if (Ntset.mem t n) then Tset.union (Ntmap.find t fst) (fstf tl)
			else Ntmap.find t fst
	in let rec update_prod ((follows,b) as acc) nt = function 
		|[] -> acc
		|(Terminal _)::tl -> update_prod acc nt tl 
		|(NonTerminal t)::tl -> 
			let acc2 = update acc t (fstf tl) in 
			let acc3 = if estNul tl then update acc2 t (Ntmap.find nt follows) else acc2 in 
			update_prod acc3 nt tl
	in 
	let step follows = 
		List.fold_left (fun acc (nt,p) -> update_prod acc nt p) (follows,false) g.rules
	in fixpoint step (empty_map g)

let add_entry (trans:transitionTable) (state1:stateND) (term:string) (state2:stateND) :transitionTable =
	if StateMap.mem state1 trans then begin 
		let map = StateMap.find state1 trans in 
		if Smap.mem term map then 
			let set = (StateSet.add state2 (Smap.find term map)) in
			let map2 = Smap.add term set map in 
			StateMap.add state1 map2 trans
		else StateMap.add state1 (Smap.add term (StateSet.singleton state2) map) trans 
		end 
	else StateMap.add state1 (Smap.add term (StateSet.singleton state2) Smap.empty) trans

let rec affichePD = function
	|[] -> print_newline ()
	|(TerminalD t)::tl -> (print_string (" \""^t^"\"");affichePD tl)
	|(NonTerminalD t)::tl -> (print_string (" "^t);affichePD tl)
	|Dot::tl -> (print_string " *";affichePD tl)

let rec pos n = function 
	|[] -> failwith "vide"
	|Dot::tl -> n 
	|_::tl -> pos (n+1) tl

let buildAutom g = 
	let nulls = null g in 
	let fst = first g nulls in 
	let flw = follow g nulls fst in
	let rec convert (l:production):productionD = match l with 
		|[] -> []
		|(Terminal t)::tl -> (TerminalD t)::convert tl
		|(NonTerminal t)::tl -> (NonTerminalD t)::convert tl 
	in let rajouteRule m (n,p) = 
		if Smap.mem n m then
			let oldSet = Smap.find n m in 
			let newSet = PDset.add (Dot::convert p) oldSet in
			Smap.add n newSet m
		else Smap.add n (PDset.singleton (Dot::convert p)) m
	in let mapDotRules = List.fold_left rajouteRule Smap.empty g.rules in
	let rec rajouteDeb e = function
		|[] -> []
		|hd::tl -> (e::hd)::rajouteDeb e tl
	in let rec listStates l = match l with
		|[] -> [[Dot]]
		|hd::tl -> (Dot::l)::rajouteDeb hd (listStates tl)
	in let rajouteRules m n p t = 
		List.fold_left (fun map pd -> StateMap.add (n,pd,t) Smap.empty map) m (listStates (convert p))
	in let rajouteRule m (n,p) = 
		let after = Ntmap.find n flw in
		if Tset.cardinal after > 0 then Tset.fold (fun t m2 -> rajouteRules m2 n p t) after m
		else if n=g.start then rajouteRules m n p "#"
		else m 
	in let mapEmpty = List.fold_left rajouteRule StateMap.empty g.rules
	in let rec miroir l1 l2 = match l1 with
		|[] -> l2
		|hd::tl -> miroir tl (hd::l2)
	in let rec following (suiv:terminal) fin = match fin with
		|[] -> Tset.singleton suiv 
		|(TerminalD t)::tl -> Tset.singleton t
		|(NonTerminalD t)::tl -> if Ntset.mem t nulls then
			Tset.union (Ntmap.find t fst) (following suiv tl)
			else Ntmap.find t fst 
		|Dot::tl -> failwith "Not possible to have a Dot in following" in
	if print_all then begin 
		print_newline ();
		Smap.iter (fun str stateSet -> print_string str;PDset.iter (fun pd -> print_string " | "; affichePD pd) stateSet;print_newline ()) mapDotRules;
		end;
	let rajouteEntry (m:transitionTable) (s1:stateND) (t:terminal) (nm:non_terminal) (pd:productionD) (set:Tset.t):transitionTable = 
		Tset.fold (fun str m2 -> add_entry m2 s1 t (nm,pd,str)) set m
	in let rec rajouteTrans (m:transitionTable) (nm:non_terminal) (deb:productionD) (fin:productionD) (suivant:terminal):transitionTable =  match fin with
		|[] -> m
		|(TerminalD t)::tl ->
			let premier = miroir deb (Dot::fin) in
			let deuxieme = miroir deb ((TerminalD t)::Dot::tl) in
			rajouteTrans (add_entry m (nm,premier,suivant) t (nm,deuxieme,suivant)) nm ((TerminalD t)::deb) tl suivant
		|(NonTerminalD t)::tl ->
			let premier = miroir deb (Dot::fin) in 
			let deuxieme = miroir deb ((NonTerminalD t)::Dot::tl) in
			let suite = Smap.find t mapDotRules in
			let m3 = PDset.fold (fun (pd:productionD) (m2:transitionTable) -> rajouteEntry m2 (nm,premier,suivant) "" t pd (following suivant tl)) suite m in
			rajouteTrans (add_entry m3 (nm,premier,suivant) t (nm,deuxieme,suivant)) nm ((NonTerminalD t)::deb) tl suivant
		|Dot::tl -> failwith "Pattern error cant have Dot in rajouteTrans"
	in let rajouteTransAll (m:transitionTable) (n:non_terminal) (p:production):transitionTable = 
		let after = Ntmap.find n flw in
		if Tset.cardinal after > 0 then Tset.fold (fun t m1 -> rajouteTrans m1 n [] (convert p) t) after m
		else if n = g.start then rajouteTrans m n [] (convert p) "#"
		else m 
	in let transition = List.fold_left (fun m (n,p) -> rajouteTransAll m n p) mapEmpty g.rules
	in let prem = Smap.find g.start mapDotRules in
	let prem2 = PDset.fold (fun pd m -> StateSet.add (g.start,pd,"#") m) prem StateSet.empty in
	{startND = prem2; transND = transition}

let successor g :successor = 
	let init r trans m = 
		if Smap.mem "" trans then 
			StateMap.add r (StateSet.add r (Smap.find "" trans)) m
		else StateMap.add r (StateSet.singleton r) m
	in let debut = StateMap.fold init g.transND StateMap.empty in
	if print_all then print_string "first calculated\n";
	let union set m =
		StateSet.fold (fun ((n,pd,suiv) as r) s -> StateSet.union s (StateMap.find r m)) set StateSet.empty
	in let step m =
		if print_all then print_string "set\n";
		let newmap = StateMap.mapi (fun (n,pd,suiv) s -> union s m) m in
		(newmap, not (StateMap.equal (fun s1 s2 -> StateSet.equal s1 s2) newmap m))
	in fixpoint step debut

let calcReal (suiv:successor) (st:state):state =
	StateSet.fold (fun ((n,pd,suv) as r) s -> StateSet.union s (StateMap.find r suiv)) st StateSet.empty

let calcNext (st:state) (suiv:successor) (transname:string) (trans:transitionTable):state =
	let next (r:stateND):state =
		let m = StateMap.find r trans in
		if Smap.mem transname m then
			Smap.find transname m
		else StateSet.empty
	in let s1 = StateSet.fold (fun r s -> StateSet.union s (next r)) st StateSet.empty
	in calcReal suiv s1

let readable (st:state) (trans:transitionTable):Sset.t = 
	let aux (r:stateND) (s:Sset.t):Sset.t =
		let transitions = StateMap.find r trans in 
		let couples = Smap.bindings transitions in 
		List.fold_left (fun s2 (n,_) -> Sset.union s2 (Sset.singleton n)) s couples
	in StateSet.fold aux st Sset.empty

let rajouteTransition (s:state) (str:string) (next:state) (m:transitionTableD) = 
	if StateSetMap.mem s m then 
		let map = StateSetMap.find s m in 
		StateSetMap.add s (Smap.add str next map) m
	else StateSetMap.add s (Smap.singleton str next) m

let nbTrans s = 
	if Smap.mem "" s then StateSet.cardinal (Smap.find "" s) else 0

let determinisation g = 
	if print_all then StateMap.iter (fun (n,pd,s) su ->print_string (n^s^" ");print_int (nbTrans su);print_string " <> ";affichePD pd) g.transND;
	let succ = successor g in
	if print_all then StateMap.iter (fun (n,pd,s) su ->print_string (n^s^" ");print_int (StateSet.cardinal su);print_string " -> ";affichePD pd) succ;
	if print_all then print_int (StateMap.cardinal succ);
	if print_all then print_string " nb succ calculated\n";
	if print_all then print_int (StateMap.cardinal g.transND);
	if print_all then print_string " for transitions\n";
	if print_all then print_string "debut calc startState\n";
	let startState = calcReal succ g.startND in
	if print_all then print_int (StateSet.cardinal startState);
	if print_all then print_string " states in startState\n";
	let rec rajouteEntree (s:state) (str:string) (m:transitionTableD) :transitionTableD = 
		if str = "" then m else 
		let next = calcNext s succ str g.transND in 
		construitTrans next (rajouteTransition s str next m)
	and construitTrans (s:state) (m:transitionTableD):transitionTableD = 
		if StateSetMap.mem s m then m else begin
			let lisibles = readable s g.transND in 
			Sset.fold (rajouteEntree s) lisibles (StateSetMap.add s Smap.empty m)
		end
	in if print_all then print_string "debut construction transitions\n";
	{startSet = startState; transitions = construitTrans startState StateSetMap.empty}

let buildAutomateD g = 
	let a = buildAutom g in
	if print_all then print_string "fin build autom non det\n";
	determinisation a

let canReduce ((n,p,suiv):stateND) = 
	let rec aux = function 
		| [] -> failwith "Empty rule"
		| [Dot] -> true
		| Dot::tl -> false
		| _::tl -> aux tl
	in aux p

let reduces (s:state):state = StateSet.filter canReduce s

let is_lr1_w_priority (a:automatD) = 
	let t = ref true in 
	let testState s suivant = 
		let reduceSet = reduces s in 
		let n =  StateSet.cardinal reduceSet in 
		if n = 0 then () else begin
			if Smap.cardinal suivant > 0 then begin
				t := false;
				print_string "reduce/shift conflict\n"
				end;
			if n > 1 then begin
				t := false;
				print_string "reduce/reduce conflict\n"
			end;
			()
		end
	in StateSetMap.iter testState a.transitions;
	!t

let giveNumbers (t:transitionTableD) = 
	let n = ref (-1) in 
	StateSetMap.map (fun _ -> n:= !n+1; !n) t

let rec buildPriorityMap n = function 
	|[] -> Ntmap.empty 
	|(_,s)::tl -> Ntmap.add s n (buildPriorityMap (n+1) tl)

let rec buildAssocMap = function 
	| [] -> Tmap.empty 
	| (a,n)::tl -> Tmap.add n a (buildAssocMap tl)

let rec unconvertPD_P = function
	|[] -> []
	|(TerminalD t)::tl -> (Terminal t)::unconvertPD_P tl 
	|(NonTerminalD t)::tl -> (NonTerminal t)::unconvertPD_P tl 
	|Dot::tl -> unconvertPD_P tl

let rec termnailsIn = function
	| [] -> Tset.empty
	|(Terminal t)::tl -> Tset.add t (termnailsIn tl)
	|(NonTerminal _)::tl -> termnailsIn tl

let rec buildTset = function
	|[] -> Tset.empty 
	|(n,p)::tl -> Tset.union (termnailsIn p) (buildTset tl)

let rec nonTermnailsIn = function
	| [] -> Ntset.empty
	|(NonTerminal t)::tl -> Tset.add t (termnailsIn tl)
	|(Terminal t)::tl -> nonTermnailsIn tl
	
let rec buildNtset = function
	|[] -> Ntset.empty 
	|(n,p)::tl -> begin 
		let dansEq = (nonTermnailsIn p) in
		if print_all && Ntset.mem "#" dansEq then print_string n;
		Ntset.union dansEq (buildNtset tl)
		end

let convertSmap_Ntmap smap = Smap.fold (fun k s m -> Ntmap.add k s m) smap Ntmap.empty

let convertSmap_Tmap smap = Smap.fold (fun k s m -> Tmap.add k s m) smap Tmap.empty

let convertTmap_action tmap = Tmap.map (fun i -> SHIFT i) tmap

let unconvertSND_SN rd = match rd with 
	|Some (n,pd,suiv) -> (n, unconvertPD_P pd, suiv)
	|None -> failwith "Should not be None"

let rec findPriority pMap = function
	|[] -> None
	|(NonTerminal t)::tl -> findPriority pMap tl
	|(Terminal t)::tl -> begin match findPriority pMap tl, Tmap.find_opt t pMap with 
		| None , None  -> None 
		| Some i, None -> Some i 
		| None, Some i -> Some i
		| Some i, Some j -> Some (min i j)
	end

let rajouteR_Rset (t:terminal) (r:rule) (m:Rset.t Tmap.t) = 
	if Tmap.mem t m then Tmap.add t (Rset.add r (Tmap.find t m)) m
	else Tmap.add t (Rset.singleton r) m

let findHighestPrio (ruleSet:Rset.t) pMap aMap:action =
	if Rset.cardinal ruleSet > 1 then failwith "reduce/reduce conflict"
	else REDUCE (Rset.choose ruleSet)

let fusionSR (shift_line:action Tmap.t) (rules:StateSet.t) pMap aMap (tset: Tset.t) =
	let ruleMap = StateSet.fold (fun (n,pd,suiv) m -> rajouteR_Rset suiv (n,unconvertPD_P pd) m) rules Tmap.empty in 
	let aux t m = match Tmap.mem t shift_line, Tmap.mem t ruleMap with
		|false, false -> m
		|true, false -> Tmap.add t (Tmap.find t shift_line) m
		|false, true -> Tmap.add t (findHighestPrio (Tmap.find t ruleMap) pMap aMap) m
		|true, true -> failwith "Choice not Implemented : shift/reduce conflict"
	in Tset.fold aux tset Tmap.empty

let buildTable (g:grammar) (priority:priority) =
	let a = buildAutomateD g in
	if print_all then print_string "Automate Deterministe fini\n";
	let ntS = buildNtset g.rules in 
	let tS = buildTset g.rules in
	let numMap = giveNumbers a.transitions in
	let priorityMap = buildPriorityMap 0 priority in
	let assocMap = buildAssocMap priority in
	(*let nbPrio = List.length priority in*)
	let buildReduceTab set _ m =
		let laws = reduces set in
		Imap.add (StateSetMap.find set numMap) laws m in
	let reductionTab = StateSetMap.fold buildReduceTab a.transitions Imap.empty in
	let convertTrans t = 
		Smap.map (fun s -> try StateSetMap.find s numMap
		with Not_found -> (failwith "l.504") ) t
	in let firstActionTab s trans m = 
		Imap.add (StateSetMap.find s numMap) (convertTrans trans) m
	in let rawTableShift = StateSetMap.fold firstActionTab a.transitions Imap.empty in 
	let estNt str i = Ntset.mem str ntS in
	let estT str i = Ntset.mem str tS in
	let gotoTable = Imap.map (fun trans -> convertSmap_Ntmap (Smap.filter estNt trans)) rawTableShift in
	let shiftTabRaw = Imap.map (fun trans -> convertSmap_Tmap (Smap.filter estT trans)) rawTableShift in
	let shiftTab = Imap.map (fun trans -> convertTmap_action trans) shiftTabRaw in
	let aux i sline m = 
		if Imap.mem i reductionTab then 
		let reduce = try Imap.find i reductionTab with Not_found -> failwith "reduce = Imap.find l.483" in
		Imap.add i (fusionSR sline reduce priorityMap assocMap tS) m
		else Imap.add i sline m 
	in let actionTab = Imap.fold aux shiftTab Imap.empty in
	{startLine = StateSetMap.find a.startSet numMap;action = actionTab; goto = gotoTable}

let calcAction (state:int) lexeme (table:actionTable) = failwith "Not Implemented"

let calcGoto (state:int) (nonTerm:non_terminal) (table:gotoTable) = failwith "Not Implemented"

let parse (p:parser) (inputs:terminal list) = failwith "Not Implemented"
	
let pp_non_terminal fmt s = Format.fprintf fmt "%s" s

let pp_iter iter pp_elt fmt =
  let first = ref true in
  iter (fun elt ->
      if not !first then Format.fprintf fmt ",@ " else first := false;
      pp_elt fmt elt)

let pp_nulls fmt = Format.fprintf fmt "@[%a@]" (pp_iter Ntset.iter pp_non_terminal)

let pp_iter_bindings iter pp_binding fmt =
  let first = ref true in
  iter (fun key elt ->
      if not !first then Format.fprintf fmt "@\n" else first := false;
      pp_binding fmt key elt)

let pp_terminal fmt s = Format.fprintf fmt "%s" s

let pp_firsts fmt =
  Format.fprintf fmt "@[%a@]"
  @@ pp_iter_bindings Ntmap.iter (fun fmt nt ts ->
         Format.fprintf fmt "@[%a -> {%a}@]" pp_non_terminal nt
           (pp_iter Tset.iter pp_terminal)
           ts)

let pp_follows = pp_firsts

let pp_symbol fmt = function
  | Terminal s -> Format.fprintf fmt "\"%s\"" s
  | NonTerminal s -> Format.fprintf fmt "%s" s

let rec pp_production fmt = function
  | [] -> ()
  | [x] -> pp_symbol fmt x
  | x :: l -> Format.fprintf fmt "%a %a" pp_symbol x pp_production l

let pp_table fmt t =
  let print_entry c n p =
    Format.fprintf fmt "  -%s:%d @[%a@]@\n" c n pp_production p in
  let print_row nt m =
       Format.fprintf fmt "@[Expansions for %s:@\n" nt;
       Tmap.iter (fun c rs -> Pset.iter (print_entry c (Pset.cardinal rs)) rs) m;
       Format.fprintf fmt "@]" in
  Ntmap.iter print_row t

let pp_goto fmt nt i = Format.fprintf fmt "\t\t%s -> %i\n" nt i

let pp_action fmt t = function
	| SHIFT i -> Format.fprintf fmt "\t\t%s -> s%i@\n" t i
	| REDUCE (n,p) -> Format.fprintf fmt "\t\t%s -> r(%s -> %a)@\n" t n pp_production p

let pp_parseTables fmt (pt:parseTables) = 
	let m1 = Imap.fold (fun i k j -> max i j) pt.action 0 in 
	let maxi = Imap.fold (fun i k j -> max i j) pt.goto m1 in
	Format.fprintf fmt "Starting line : %i\n" pt.startLine;
	for i = 0 to maxi do 
		Format.fprintf fmt "Map for %i :@\n" i;
		if Imap.mem i pt.action then
			let action = Imap.find i pt.action in 
			if Tmap.cardinal action > 0 then begin
				Format.fprintf fmt "\tActions :@\n";
				Tmap.iter (fun t a -> pp_action fmt t a) (Imap.find i pt.action);
				end;
		if Imap.mem i pt.goto then 
			let goto = Imap.find i pt.goto in 
			if Ntmap.cardinal goto > 0 then begin 
				Format.fprintf fmt "\tGoto :@\n";
				Ntmap.iter (fun nt i -> pp_goto fmt nt i) goto;
				end
	done;;


if print_all then print_string "arith\n";;
let table_arith = buildTable g_arith [];;
if print_all then print_string "1\n";;
let table1 = buildTable g1 [];;
if print_all then print_string "gram\n";;
let table_gram = buildTable g_gram [];;
if print_all then print_string "cours\n";;
let table_cours = buildTable g_cours [];;

let () = Format.printf "%a@." pp_parseTables table_cours


