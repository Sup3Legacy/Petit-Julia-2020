(*
##########################################
#                                        #
#     Fichier principal de Samenhir      #
#                                        #
# Contient le corp et l'âme de Samenhir  #
#                                        #
##########################################
*)


(* Le début vient du TD 6 sur l'analyse descendente, l'algorithme utilisé si dessous est l'algorithme de Knuth *)
let print_all = ref false
let explain = ref false
let endString = "Not_a_token"

open Lexing
open Format
open SamenhirAst

(* Calcul le point fixe de f *)
let fixpoint f x =
	let t = ref true in 
	let x2 = ref x in 
	while !t do
		let (x3,t2) = f !x2 in 
		x2 := x3;
		t := t2;
	done;
	!x2;;

(* Teste si une production peut reconnaitre le mot vide en utilisant une table des règle reconnaissant le mot vide *)
let is_null_production (valNulles:nulls) (prod:production) = 
	let rec aux = function
		| [] -> true
		| hd::tl -> begin
			match hd with
			| NonTerminal _ -> false
			| Terminal t -> (Ntset.mem t valNulles)&&(aux tl)
		end
	in aux prod

(* Calcule la table des productions reconnaissant le mot vide *)
let null g :nulls = 
	let step nulls = 
		let nulSet = ref nulls in 
		let t = ref false in
		let aux (nom, prod, _) = 
			if Ntset.mem nom nulls then () else begin
				if is_null_production nulls prod then begin
						nulSet := Ntset.add nom !nulSet;
						t := true
				end
			end
		in List.iter aux g.rules;
		(!nulSet, !t)
	in fixpoint step Ntset.empty

(* Construit une table vide *)
let empty_map g = 
	let dico = ref Ntmap.empty in 
	List.iter (fun (n, _, _) -> dico := Ntmap.add n Tset.empty !dico) g.rules;
	!dico

(* Calcule le premier Terminal d'une production à partir des table des règles reconnaissant le mot vide et des premiers Terminal des règles *)
let first_production_step (valNulles:nulls) (fst:first) (prod:production) = 
	let rec aux = function
		|[] -> Tset.empty
		|(Terminal t)::tl -> Tset.singleton t 
		|(NonTerminal t)::tl -> if Ntset.mem t valNulles
			then Tset.add t (aux tl)
			else (try Ntmap.find t fst with Not_found -> (print_string t;print_string " wasn't found\n"; raise Not_found))
	in aux prod

(* Calcule les premiers Terminaux possible d'une règle *)
let first g (nul:nulls) :first = 
	let empty = empty_map g in 
	if !print_all then begin 
		print_int (Ntmap.cardinal empty);
		print_string " ";
		print_int (List.length g.rules);
		print_newline () end;
	let step fst = 
		let aux fst2 (nom, prod, _) = 
			Ntmap.add
				nom
				(Tset.union
					(try Ntmap.find nom fst2 with Not_found -> failwith " haven't found nom" | a -> failwith "unkwon error")
					(try first_production_step nul fst prod with Not_found -> (print_string " - not_found in first_production_step\n"; raise Not_found))
				)
				fst2
		in let fstMap = try List.fold_left aux empty g.rules with Not_found -> (print_string " - not_found in List.fold_left\n"; raise Not_found) in
		(fstMap, not (Ntmap.equal Tset.subset fstMap fst)) in
	fixpoint step empty

(* Calcule tous les Terminaux qui peuvent suivre une règle *)
let follow g (n:nulls) (fst:first) :follow = 
	let update (follows,b) nt follow_nt= 
		let old = (try Ntmap.find nt follows with Not_found -> failwith ("undefined non_terminal "^nt)) in 
		(Ntmap.add nt (Tset.union follow_nt (try Ntmap.find nt follows with Not_found -> failwith ("undefined non_terminal "^nt))) follows, b || not (Tset.subset follow_nt old))
	in let rec estNul = function
		|[] -> true
		|(Terminal _)::tl -> false
		|(NonTerminal t)::tl -> (Ntset.mem t n) && estNul tl
	in let rec fstf = function
		|[] -> Tset.empty
		|(Terminal t)::tl -> Tset.singleton t
		|(NonTerminal t)::tl -> if (Ntset.mem t n) then Tset.union (try Ntmap.find t fst with Not_found -> failwith ("undefined non_terminal "^t^" in firsts")) (fstf tl)
			else (try Ntmap.find t fst with Not_found -> failwith ("undefined non_terminal "^t^" in firsts"))
	in let rec update_prod ((follows,b) as acc) nt = function 
		|[] -> acc
		|(Terminal _)::tl -> update_prod acc nt tl 
		|(NonTerminal t)::tl -> 
			let acc2 = update acc t (fstf tl) in 
			let acc3 = if estNul tl then update acc2 t (try Ntmap.find nt follows with Not_found -> failwith ("undefined non_terminal "^t^" in follows")) else acc2 in 
			update_prod acc3 nt tl
	in 
	let step follows = 
		if !print_all then print_string "step follow\n";
		List.fold_left (fun acc (nt, p, _) -> update_prod acc nt p) (follows,false) g.rules
	in 
	fixpoint step (Ntmap.mapi (fun nt s -> if nt = g.start then Tset.add endString s else s) (empty_map g))

(* Rajoute la transition de l'état state1 vers l'état state2 avec le mot term qui est un terminal, un non terminal ou la chaine de caractère vide *)
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

(* fonctions d'affichage *)
let rec afficheP = function
	|[] -> print_newline ()
	|(Terminal t)::tl -> (print_string (" \""^t^"\"");afficheP tl)
	|(NonTerminal t)::tl -> (print_string (" "^t);afficheP tl)

let rec affichePD = function
	|[] -> print_newline ()
	|(TerminalD t)::tl -> (print_string (" \""^t^"\"");affichePD tl)
	|(NonTerminalD t)::tl -> (print_string (" "^t);affichePD tl)
	|Dot::tl -> (print_string " *";affichePD tl)

let afficheState s = 
	StateSet.iter (fun (nt, pd, _, t) -> print_string (nt^" - "^t^" -> "); affichePD pd) s

let rec pos n = function 
	|[] -> failwith "vide"
	|Dot::tl -> n 
	|_::tl -> pos (n+1) tl

(* Construction de l'automate non déterministe présenté slides 82 du cours analyse syntaxique (1/2) *)
let buildAutom g = 
	if !print_all then print_string "starting\n";
	let nulls = null g in 
	if !print_all then print_int (Ntset.cardinal nulls);
	if !print_all then print_string " in null\n";
	let fst = try first g nulls with Not_found -> (print_string " - not_found in first\n"; raise Not_found) in 
	if !print_all then print_string "follow\n";
	let flw = follow g nulls fst in
	if !print_all then print_string "precalculus of followers finished\n";
	let rec convert (l:production):productionD = match l with (* converti une production au type productionD pour y ajouter un point de positionement *)
		|[] -> []
		|(Terminal t)::tl -> (TerminalD t)::convert tl
		|(NonTerminal t)::tl -> (NonTerminalD t)::convert tl 
	in let rajouteRule m (n, p, prio) =
		if Smap.mem n m then
			let oldSet = Smap.find n m in 
			let newSet = PDset.add (Dot::convert p, prio) oldSet in
			Smap.add n newSet m
		else Smap.add n (PDset.singleton (Dot::convert p, prio)) m
	in let mapDotRules = List.fold_left rajouteRule Smap.empty g.rules in (* table de toutes les noms de règles vers leur productions commencent par un point *)
	let rec rajouteDeb e = function
		|[] -> []
		|hd::tl -> (e::hd)::rajouteDeb e tl
	in let rec listStates l = match l with
		|[] -> [[Dot]]
		|hd::tl -> (Dot::l)::rajouteDeb hd (listStates tl)
	in let rajouteRules m n p prio t = 
		List.fold_left (fun map pd -> StateMap.add (n, pd, prio, t) Smap.empty map) m (listStates (convert p))
	in let rajouteRule m (n, p, prio) = 
		let after = Ntmap.find n flw in
		if Tset.cardinal after > 0 then Tset.fold (fun t m2 -> rajouteRules m2 n p prio t) after m
		else if n=g.start then rajouteRules m n p prio endString (* si la régle n'est suivi par rien et que c'est la règle de départ on met que l'on peut terminer avec ses réductions *)
		else m 
	in let mapEmpty = List.fold_left rajouteRule StateMap.empty g.rules (* construit la table avec juste toues les états et aucune transition *)
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
	let rajouteEntry (m:transitionTable) (s1:stateND) (t:terminal) (nm:non_terminal) (pd:productionD) (prio:string option) (set:Tset.t):transitionTable = 
		Tset.fold (fun str m2 -> add_entry m2 s1 t (nm, pd, prio, str)) set m
	in let rec rajouteTrans (m:transitionTable) (nm:non_terminal) (deb:productionD) (fin:productionD) (prio:string option) (suivant:terminal):transitionTable =  match fin with
		|[] -> m
		|(TerminalD t)::tl ->
			let premier = miroir deb (Dot::fin) in
			let deuxieme = miroir deb ((TerminalD t)::Dot::tl) in
			rajouteTrans (add_entry m (nm, premier, prio ,suivant) t (nm, deuxieme, prio, suivant)) nm ((TerminalD t)::deb) tl prio suivant
		|(NonTerminalD t)::tl ->
			let premier = miroir deb (Dot::fin) in 
			let deuxieme = miroir deb ((NonTerminalD t)::Dot::tl) in
			let suite = Smap.find t mapDotRules in
			let m3 = PDset.fold (fun (pd, prio2) (m2:transitionTable) -> rajouteEntry m2 (nm, premier, prio, suivant) "" t pd prio2 (following suivant tl)) suite m in
			rajouteTrans (add_entry m3 (nm, premier, prio, suivant) t (nm,deuxieme, prio, suivant)) nm ((NonTerminalD t)::deb) tl prio suivant
		|Dot::tl -> failwith "Pattern error cant have Dot in rajouteTrans"
	in let rajouteTransAll (m:transitionTable) (n:non_terminal) (p:production) (prio:string option) :transitionTable = 
		let after = Ntmap.find n flw in
		if Tset.cardinal after > 0 then Tset.fold (fun t m1 -> rajouteTrans m1 n [] (convert p) prio t) after m
		else if n = g.start then rajouteTrans m n [] (convert p) prio endString
		else m  in
	let transition = List.fold_left (fun m (n, p, prio) -> rajouteTransAll m n p prio) mapEmpty g.rules in (* rajoute toutes les transitions dans l'automate *)
	if !print_all then print_string "transitions non det finished\n";
	let prem = try Smap.find g.start mapDotRules with Not_found -> failwith ("First rule "^g.start^" not found in mapDotRules") in (* calcule l'ensemble des productions de la règle de départ *)
	let prem2 = PDset.fold (fun (pd,prio) m -> StateSet.add (g.start, pd, prio, endString) m) prem StateSet.empty in (* rajoute leur nom à ces productions *)
	if !print_all then print_string "non deterministic automaton calculated\n";
	{startND = prem2; transND = transition} (* renvoie l'automate non déterministe *)

(* Calcule la table de tous les états acceccible depuis un état par epsilon-transitions *)
let successor g :successor = 
	let init ((n,_,_,suiv) as r) trans m = 
		if Smap.mem "" trans then 
			StateMap.add r (StateSet.add r (Smap.find "" trans)) m
		else StateMap.add r (StateSet.singleton r) m
	in let debut = StateMap.fold init g.transND StateMap.empty in
	if !print_all then print_string "first calculated\n";
	let union set m =
		StateSet.fold (fun ((n, _, _, suiv) as r) s -> StateSet.union s (try StateMap.find r m with Not_found -> failwith (n^" ["^suiv^"] not found in successor"))) set StateSet.empty
	in let step m =
		if !print_all then print_string "set\n";
		let newmap = StateMap.mapi (fun _ s -> union s m) m in
		(newmap, not (StateMap.equal (fun s1 s2 -> StateSet.equal s1 s2) newmap m))
	in fixpoint step debut

(* Calcule l'état réel de l'automate déterministe en ajoutant à l'ensemble d'états tous ceux accessibles par epsilon-transitions *)
let calcReal (suiv:successor) (st:state):state =
	StateSet.fold (fun r s -> StateSet.union s (StateMap.find r suiv)) st StateSet.empty

(* Calcule l'état d'arrivé par une transition *)
let calcNext (st:state) (suiv:successor) (transname:string) (trans:transitionTable):state =
	let next (r:stateND):state =
		let m = StateMap.find r trans in
		if Smap.mem transname m then
			Smap.find transname m
		else StateSet.empty
	in let s1 = StateSet.fold (fun r s -> StateSet.union s (next r)) st StateSet.empty
	in calcReal suiv s1

(* Calcule tous les terminaux et non terminaux lisibles depuis un état *)
let readable (st:state) (trans:transitionTable):Sset.t = 
	let aux (r:stateND) (s:Sset.t):Sset.t =
		let transitions = StateMap.find r trans in 
		let couples = Smap.bindings transitions in 
		List.fold_left (fun s2 (n,_) -> Sset.union s2 (Sset.singleton n)) s couples
	in StateSet.fold aux st Sset.empty

(* Rajoute une transition dans la talbe de trnasition de l'automate déterministe *)
let rajouteTransition (s:state) (str:string) (next:state) (m:transitionTableD) = 
	if StateSetMap.mem s m then 
		let map = StateSetMap.find s m in 
		StateSetMap.add s (Smap.add str next map) m
	else StateSetMap.add s (Smap.singleton str next) m

(* Déterminisation de l'automate g (effectivement c'est un nom bizarre pour un automate) *)
let determinisation g = 
	let succ = successor g in
	if !print_all then print_int (StateMap.cardinal succ);
	if !print_all then print_string " nb succ calculated\n";
	if !print_all then print_int (StateMap.cardinal g.transND);
	if !print_all then print_string " for transitions\n";
	if !print_all then print_string "debut calc startState\n";
	let startState = calcReal succ g.startND in
	if !print_all then print_int (StateSet.cardinal startState);
	if !print_all then print_string " states in startState\n";
	let rec rajouteEntree (s:state) (str:string) (m:transitionTableD) :transitionTableD = 
		if str = "" then m else 
		let next = calcNext s succ str g.transND in 
		construitTrans next (rajouteTransition s str next m)
	and construitTrans (s:state) (m:transitionTableD):transitionTableD = 
		if StateSetMap.mem s m then m else begin
			let lisibles = readable s g.transND in 
			Sset.fold (rajouteEntree s) lisibles (StateSetMap.add s Smap.empty m)
		end
	in if !print_all then print_string "debut construction transitions\n";
	{startSet = startState; transitions = construitTrans startState StateSetMap.empty}

(* Construit l'automate déterministe à partir de la grammaire en appelant buildAutom et determinisation *)
let buildAutomateD g = 
	let a = buildAutom g in
	if !print_all then print_string "fin build autom non det\n";
	determinisation a

(* Regarde si la production est dans un état autorisant la réduction de la règle associé *)
let canReduce ((n, p, suiv, _):stateND) = 
	let rec aux = function 
		| [] -> failwith "Empty rule"
		| [Dot] -> true
		| Dot::tl -> false
		| _::tl -> aux tl
	in aux p

(* Récupère dans un états toutes les productions réductibles *)
let reduces (s:state):state = StateSet.filter canReduce s

(* Numérote les états pour pouvoir en faire une table *)
let giveNumbers (t:transitionTableD) = 
	let n = ref 0 in 
	StateSetMap.map (fun _ -> n:= !n+1; !n) t

(* Crée des tables pour trouver les prioritées et associativité facilement *)
let rec buildPriorityMap n = function 
	|[] -> Ntmap.empty 
	|(_,sL)::tl -> List.fold_left (fun m str -> Ntmap.add str n m) (buildPriorityMap (n+1) tl) sL

let rec buildAssocMap = function 
	| [] -> Tmap.empty 
	| (a,nL)::tl -> List.fold_left (fun m str -> Tmap.add str a m) (buildAssocMap tl) nL

(* Reconvertie les production en la forme initiale *)
let rec unconvertPD_P = function
	|[] -> []
	|(TerminalD t)::tl -> (Terminal t)::unconvertPD_P tl 
	|(NonTerminalD t)::tl -> (NonTerminal t)::unconvertPD_P tl 
	|Dot::tl -> unconvertPD_P tl

(* Construit l'ensemble des terminaux existant *)
let rec terminalsIn = function
	| [] -> Tset.empty
	|(Terminal t)::tl -> Tset.add t (terminalsIn tl)
	|(NonTerminal _)::tl -> terminalsIn tl

let rec buildTset = function
	|[] -> Tset.empty 
	|(n, p, _)::tl -> Tset.union (terminalsIn p) (buildTset tl)

(* Construit l'ensemble des non terminaux *)
let rec nonTermnailsIn = function
	| [] -> Ntset.empty
	|(NonTerminal t)::tl -> Tset.add t (nonTermnailsIn tl)
	|(Terminal t)::tl -> nonTermnailsIn tl

let rec buildNtset = function
	|[] -> Ntset.empty 
	|(n, p, _)::tl -> begin 
		let dansEq = (nonTermnailsIn p) in
		if !print_all && Ntset.mem endString dansEq then print_string n;
		Ntset.union dansEq (buildNtset tl)
		end

(* Fait de nombreuse conversion d'un type de structure vers une autre *)
let convertSmap_Ntmap smap = Smap.fold (fun k s m -> Ntmap.add k s m) smap Ntmap.empty

let convertSmap_Tmap smap = Smap.fold (fun k s m -> Tmap.add k s m) smap Tmap.empty

let convertTmap_action tmap = Tmap.map (fun i -> SHIFT i) tmap

let unconvertSND_SN rd = match rd with 
	|Some (n,pd,suiv) -> (n, unconvertPD_P pd, suiv)
	|None -> failwith "Should not be None"

(* rajoute une règle à une table de règles *)
let rajouteR_Rset (t:terminal) (r:rule) (m:Rset.t Tmap.t) = 
	if Tmap.mem t m then Tmap.add t (Rset.add r (Tmap.find t m)) m
	else Tmap.add t (Rset.singleton r) m

(* Calcule le coefficient de priorité d'une règle *)
let rec calcPrioPD pMap = function
	| [] -> failwith "empty rule"
	| [Dot] -> max_int
	| Dot::tl -> failwith "not a rule"
	| NonTerminalD _::tl -> calcPrioPD pMap tl
	| TerminalD t::tl -> min (calcPrioPD pMap tl) (Tmap.find t pMap)

let rec calcPrioP pMap = function
	| [] -> max_int
	| NonTerminal _::tl -> calcPrioP pMap tl
	| Terminal t::tl -> min (calcPrioP pMap tl) (try Tmap.find t pMap with Not_found -> max_int)

let calcPrio pMap (p:production) = function
	|None -> (try calcPrioP pMap p with Not_found -> assert false)
	|Some t -> (try Tmap.find t pMap with Not_found -> assert false)

(* Cherche la règle avec la priorité la plus importante pour savoir laquel réduire *)
let findHighestPrio (ruleSet:Rset.t) pMap:action =
	if Rset.cardinal ruleSet > 1 then 
		let v =  Rset.fold (fun (n, p, prio) m -> let v = calcPrio pMap p prio in
			match m with
				|None -> Some (v, n, p, prio)
				|Some (v2,n2,p2,prio2) -> if v < v2 then Some (v2, n2, p2, prio2) else Some (v, n, p, prio)
			) ruleSet None in
		match v with
			|None -> assert false
			|Some (_, n, p, prio) -> REDUCE (n, p, prio)
	else REDUCE (Rset.choose ruleSet)

(* Cherche le non terminal dont la priorité est la plus importante (pour l'associativité) *)
let findPrioToken p pMap = 
	let rec aux i l = function
		|[] -> l
		| NonTerminal _::tl -> aux i l tl
		| Terminal t::tl -> if Tmap.mem t pMap then let v = Tmap.find t pMap in if v > i then aux v (Some t) tl else aux i l tl else aux i l tl
	in match aux min_int None p with
		|None -> (afficheP p; failwith "can't solve conflict because no prio token")
		|Some t -> t

(* Fusionne la table des shift et celle des réduction pour faire la table des action depuis un état précis *)
let fusionSR (shift_line:action Tmap.t) (rules:StateSet.t) pMap aMap (tset: Tset.t) =
	let ruleMap = StateSet.fold (fun (n, pd, prio, suiv) m -> rajouteR_Rset suiv (n, unconvertPD_P pd, prio) m) rules Tmap.empty in 
	let aux t m = match Tmap.mem t shift_line, Tmap.mem t ruleMap with
		|false, false -> m
		|true, false -> (try Tmap.add t (Tmap.find t shift_line) m with Not_found -> assert false)
		|false, true -> (try Tmap.add t (findHighestPrio (Tmap.find t ruleMap) pMap) m with Not_found -> assert false)
		|true, true -> 
			Tmap.add t (
				let (n, p, prio) = match findHighestPrio (try Tmap.find t ruleMap with Not_found -> assert false) pMap with
					|REDUCE a -> a
					| _ -> assert false
				in let priorityToken = match prio with
					|None -> findPrioToken p pMap
					|Some t -> t
				in let pS = (try Tmap.find t pMap with Not_found -> (afficheP p; failwith ("can't solve a shift/reduce conflict because \""^t^"\" has no associated priority")))
				in let pR = Tmap.find priorityToken pMap in
				if pS > pR then Tmap.find t shift_line
				else if pS < pR then REDUCE (n, p, prio)
				else match Tmap.find t aMap with
					|Left -> REDUCE (n, p, prio)
					|Right -> Tmap.find t shift_line
					|NonAssoc -> failwith "can't solve conflict"
			) m
	in Tset.fold aux tset Tmap.empty

(*
let rajouteAction (i:int) (t:terminal) (act:action) (m:actionTable) = 
	if Imap.mem i m then
		let map = Imap.find i m in 
		Imap.add i (Tmap.add t act map) m
	else Imap.add i (Tmap.add t act Tmap.empty) m

let firstNT g = 
	let rec aux = function
		|[] -> failwith "inexistant rule"
		|(n, p, _)::tl when n = g.start -> begin
			match p with
			| [] -> failwith "empty first rule"
			|(Terminal t)::tl -> failwith "wrong format first rule"
			|(NonTerminal t)::tl -> t
			end
		|_::tl -> aux tl
	in aux g.rules*)

(* fonctions d'affichage de débug *)
let rec affichePD_Fmt fmt = function
	|[] -> ()
	|TerminalD t::tl -> Format.fprintf fmt "\"%s\" %a" t affichePD_Fmt tl
	|NonTerminalD t::tl -> Format.fprintf fmt "%s %a" t affichePD_Fmt tl
	|Dot::tl -> Format.fprintf fmt "* %a" affichePD_Fmt tl

let afficheFichierEtats nM = 
	let out = open_out "parser.explain" in
	let fmt = Format.formatter_of_out_channel out in
	StateSetMap.iter (fun k i ->
		Format.fprintf fmt "Etat : %i\n" i;
		StateSet.iter (fun (nT, prodD, _, term) -> 
			Format.fprintf fmt " - %s[%s] -> %a\n" nT term affichePD_Fmt prodD) k) nM;
	close_out out

(* construction de la table *)
let buildTable (g:grammar) (priority:priority) =
	let a = buildAutomateD g in
	if !print_all then print_string "Automate Deterministe fini\n";
	let ntS = buildNtset g.rules in 
	if !print_all then print_string "builded non-terminal set\n";
	let tS = Tset.add endString (buildTset g.rules) in
	if !print_all then print_string "builded terminal set\n";
	let numMap = giveNumbers a.transitions in
	if !print_all then print_string "Builded number map\n";
	if !explain then afficheFichierEtats numMap;
	let priorityMap = buildPriorityMap 0 priority in
	if !print_all then print_string "Builded priority map\n";
	let assocMap = buildAssocMap priority in
	if !print_all then print_string "Debut build reductionTab\n";
	let buildReduceTab set _ m =
		let laws = reduces set in
		Imap.add (try StateSetMap.find set numMap with Not_found -> assert false) laws m in
	let reductionTab = StateSetMap.fold buildReduceTab a.transitions Imap.empty in
	if !print_all then print_string "Fin build reductionTab\n";
	let convertTrans t = 
		Smap.map (fun s -> try StateSetMap.find s numMap
		with Not_found -> assert false ) t
	in let firstActionTab s trans m = 
		Imap.add (try StateSetMap.find s numMap with Not_found -> assert false) (convertTrans trans) m
	in let rawTableShift = StateSetMap.fold firstActionTab a.transitions Imap.empty in 
	if !print_all then print_string "Builded rawTableShift\n";
	let estNt str i = Ntset.mem str ntS in
	let estT str i = Ntset.mem str tS in
	let gotoTable = Imap.map (fun trans -> (Smap.filter estNt trans)) rawTableShift in
	let shiftTabRaw = Imap.map (fun trans -> convertSmap_Tmap (Smap.filter estT trans)) rawTableShift in
	let shiftTab = Imap.map (fun trans -> convertTmap_action trans) shiftTabRaw in
	let aux i sline m =
		if Imap.mem i reductionTab then
		let reduce = try Imap.find i reductionTab with Not_found -> failwith "reduce = Imap.find l.483" in
		Imap.add i (fusionSR sline reduce priorityMap assocMap tS) m
		else Imap.add i sline m 
	in let actionTab = Imap.fold aux shiftTab Imap.empty in
	let starting = StateSetMap.find a.startSet numMap in
	{startLine = starting;action = actionTab; goto = gotoTable}

(* Convertie une grammaire de l'état lu par le parser faire la structure permettant de construire la table *)
let rec unRawProd = function
	|[] -> []
	|TerminalR t::tl -> Terminal t::unRawProd tl
	|NonTerminalR t::tl -> NonTerminal t::unRawProd tl
	|AssocTerminal (v,t)::tl -> Terminal t::unRawProd tl
	|AssocNonTerminal (v,t)::tl -> NonTerminal t::unRawProd tl

let unrawGrammar (g:grammar_raw):grammar = {start = g.startR; rules = List.fold_left (fun l (n,_,pr,prio,_) -> (n, unRawProd pr, prio)::l) [] g.raw_rules}

(* Cherche l'état de la règle de départ *)
let rec findType start = function
	|[] -> failwith "non existing start rule"
	|(nom, t, _, _, _)::tl -> if nom = start then t else findType start tl

(** Pretty printer du fichier .ml **)

(* affichage des types*)
let pp_declarationTypes (fmt:Format.formatter) p = 
	Format.fprintf fmt "type rulesType =\n";
	let nameMap = List.fold_left (fun set (nm,t,_,_,_) -> Smap.add nm t set) Smap.empty p.gR.raw_rules in 
	Smap.iter (fun nm t -> Format.fprintf fmt "\t| %s  of (%s)\n" (String.uppercase_ascii nm) t) nameMap;
	Format.fprintf fmt "\t|Tok of token\n";
	let t = findType p.gR.startR p.gR.raw_rules in 
	Format.fprintf fmt "exception Output of (%s)\n" t;
	Format.fprintf fmt "exception FailureParse of rulesType list\n";
	Format.fprintf fmt "
type actionTypes =
	|Action of int
	|Shift of int
	|Success
";;

let pp_tokenDecl fmt liste = 
	Format.fprintf fmt "type token =\n";
	Format.fprintf fmt "\t|Not_a_token\n";
	let afficheToken (t,dataT) = match dataT with 
		|None -> Format.fprintf fmt "\t|%s\n" t
		|Some data -> Format.fprintf fmt "\t|%s of (%s)\n" t data
	in List.iter (fun t -> afficheToken t) liste

(* affichage de l'entête *)
let pp_header fmt str =
	Format.fprintf fmt "%s\n\n" str;
	Format.fprintf fmt "exception Samenhir_Parsing_Error of string list\nlet samFail i = raise (Samenhir_Parsing_Error i)\n"

(* affichage de la fin avec la fonction qui lance la parseur *)
let pp_end fin fmt startS =
	Format.fprintf fmt "let %s lexer lexbuf =
	let newTok = (fun () -> lexer lexbuf) in
	try 
		_sam%i [%i] [] newTok (newTok ())
	with Output a -> a
		|a -> raise a
" fin startS startS;;

let pp_mainEnd fmt fin startS =
	Format.fprintf fmt "let %s lexer lexbuf =
	let newTok = (fun () -> lexer lexbuf) in
	try match actionAll newTok %i (newTok ()) [] [%i] with 
		|%s a -> a
		| _ -> assert false
	with Output a -> a
		| _ -> raise (Samenhir_Parsing_Error [\"Try something else\"])
" fin startS startS (String.uppercase_ascii fin);;

(* affichage des actions de réduction *)
let pp_reduce b i fmt (raw_prod,cons) =
	let rec nomVariables = function
		|[] -> ()
		|symb::tl -> begin
			nomVariables tl;
			match symb with
				|NonTerminalR _ | TerminalR _ -> ()
				| AssocTerminal (nm, _) -> Format.fprintf fmt "%s," nm
				| AssocNonTerminal (nm, _) -> Format.fprintf fmt "%s," nm
		end
	in
	let rec depile n = function
		|[] -> ()
		|symb::tl -> begin
			depile (n+1) tl;
			match symb with
				|NonTerminalR _ | TerminalR _ -> Format.fprintf fmt "_::"
				|AssocTerminal (_, t) -> Format.fprintf fmt "(Tok (%s t%i))::" (String.uppercase_ascii t) n
				|AssocNonTerminal (_, t) -> Format.fprintf fmt "(%s t%i)::" (String.uppercase_ascii t) n
		end
	in
	let rec depile2 n = function
		|[] -> ()
		|symb::tl -> begin
			depile2 (n+1) tl;
			match symb with
				|NonTerminalR _ | TerminalR _ -> ()
				|AssocTerminal _ |AssocNonTerminal _ -> Format.fprintf fmt "t%i," n
		end
	in let n = List.length raw_prod in
	if b then 
		if n = 0
		then Format.fprintf fmt "\t\t\tlet _e=%i::_e in\n" i
		else if n > 1 then begin 
			Format.fprintf fmt "\t\t\tlet _e = match _e with |";
			for i = 1 to n -1 do 
				Format.fprintf fmt "_::";
			done;
			Format.fprintf fmt "t->t|_->assert false in\n";
			end;
	if raw_prod != [] then begin
		Format.fprintf fmt "\t\t\tlet ";
		nomVariables raw_prod;
		Format.fprintf fmt "_m = match _m with |";
		depile 0 raw_prod;
		Format.fprintf fmt "tl->";
		depile2 0 raw_prod;
		Format.fprintf fmt "tl |_->assert false in\n"
		end;
	Format.fprintf fmt "\t\t\tlet valeur=(%s)in\n" cons

(* affichage de toutes les actions (shift et reduce ) depuis un état *)
let pp_action fmt starter pos t a rMap tokenTypeMap = 
	let t2 = if t = endString then "EOF" else t in
	if Tmap.mem t tokenTypeMap then begin
		Format.fprintf fmt "\t|(%s data)->\n" t2;
		 match a with
			| SUCCESS -> assert false
			| SHIFT i -> Format.fprintf fmt "\t\t_sam%i(%i::_e)(Tok(%s data)::_m)newToken(newToken())" i pos t2
			| REDUCE ((n, p, _) as r) -> begin
				pp_reduce true pos fmt (Rmap.find r rMap);
				Format.fprintf fmt "\t\tgoto(List.hd _e)\"%s\"_e((%s valeur)::_m)newToken(%s data)" n (String.uppercase_ascii n) t2

			end
		end
	else begin
		Format.fprintf fmt "\t|%s->\n" t2;
		match a with 
			| SUCCESS -> assert false
			| SHIFT i -> Format.fprintf fmt "\t\t_sam%i(%i::_e)(Tok %s::_m)newToken(newToken())" i pos t2
			| REDUCE ((n, p, _) as r) -> begin
				pp_reduce (t<>endString || n <> starter) pos fmt (Rmap.find r rMap);
				if t = endString  && n = starter then Format.fprintf fmt "\t\traise(Output valeur)"
				else Format.fprintf fmt "\t\tgoto(List.hd _e)\"%s\"_e((%s valeur)::_m)newToken %s" n (String.uppercase_ascii n) t2
				end 
		end;
	Format.fprintf fmt "\n"

let compteurReduce = ref 0
let hash = Hashtbl.create 1024

let pp_action_affiche fmt num t action (rMap:(raw_production * string) Rmap.t) tokenTypeMap = 
	let t2 = if t = endString then "EOF" else t in
	if Tmap.mem t tokenTypeMap then
		Format.fprintf fmt "\t|%i, %s _ -> " num t2
	else Format.fprintf fmt "\t|%i, %s -> " num t2;
	match action with
		| SUCCESS -> Format.fprintf fmt "Success\n"
		| SHIFT i -> Format.fprintf fmt "Shift %i\n" i
		| REDUCE r -> begin
			if Hashtbl.mem hash r then
				let (i, _) = Hashtbl.find hash r in Format.fprintf fmt "Action %i\n" i
			else begin
				let i = !compteurReduce in 
				compteurReduce := i +1;
				let red = Rmap.find r rMap in
				Hashtbl.add hash r (i, red);
				Format.fprintf fmt "Action %i\n" i
			end
		end
;;

let pp_action_table fmt action rMap tokenTypeMap =
	Format.fprintf fmt "let actionTable state nexToken = match state, nexToken with\n";
	Imap.iter (fun i -> Tmap.iter (fun n act -> pp_action_affiche fmt i n act rMap tokenTypeMap)) action;
	Format.fprintf fmt "\t| i, _ -> raise (Samenhir_Parsing_Error [\"string_of_int i\"])\n\n"
;;

let pp_reduce_action fmt tokenTypeMap (n,_,_) (num, (raw_prod, cons)) =
	let rec aux1 l = if l==[] then "","tlE" else  
		let s1,s2 = aux1 (List.tl l) in
		match List.hd l with
			|TerminalR t -> ((s1^"Tok ("^(String.uppercase_ascii t)^(if Tmap.mem t tokenTypeMap then " _" else "")^")::"),("_::"^s2))
			|AssocTerminal (s, t) -> ((s1^"Tok ("^String.uppercase_ascii t^" "^s^")::"),("_::"^s2))
			|NonTerminalR t -> ((s1^(String.uppercase_ascii t)^(if Tmap.mem t tokenTypeMap then " _" else "")^"::"),("_::"^s2))
			|AssocNonTerminal (s,t) -> ((s1^(String.uppercase_ascii t)^" "^s^"::"),("_::"^s2))
	in let s1,s2 = aux1 raw_prod in
	Format.fprintf fmt "\t|%i, %stlP, %s -> (%s (%s))::tlP,tlE\n" num s1 s2 (String.uppercase_ascii n) cons
;;

let pp_reduce_table fmt tokenTypeMap = 
	Format.fprintf fmt "and reduce (num:int) (listE:int list) (pile:rulesType list) = match num, pile, listE with\n";
	Hashtbl.iter (pp_reduce_action fmt tokenTypeMap) hash;
	Format.fprintf fmt "\t| i, _, _ ->(print_int i;print_newline ();print_int (List.length listE);print_newline ();print_int (List.length pile);print_newline (); assert false)\n\n"
;;

let pp_mainProgram fmt = Format.fprintf fmt "
let rec actionAll newToken etat nexToken pile lEtat = (*print_string \"etat : \";print_int etat;print_newline ();*)match actionTable etat nexToken with
	|Success -> begin match pile with
		|[x] -> x
		|_ -> assert false end
	|Action i ->begin let pile2, letat2 = reduce i (etat::lEtat) pile in 
		let i2 = goto letat2 pile2 in
		actionAll newToken i2 nexToken pile2 letat2 end
	|Shift i -> begin actionAll newToken i (newToken ()) (Tok nexToken::pile) (etat::lEtat) end
;;\n\n
"

(* affiche la liste de string *)
let rec afficheListStr fmt = function
	|[] -> ()
	|[s] -> Format.fprintf fmt "\"%s\"" s
	|hd::tl -> Format.fprintf fmt "\"%s\";%a" hd afficheListStr tl

(* affiche la fonction de l'état et si on lit un token impossible à lire, on renvoie la liste des nom (string) des terminaux et non terminaux qu'on aurait pu lire *)
let pp_actionStates fmt startR i actionT ruleMap tokenTypeMap goto = 
	Format.fprintf fmt (if i = 1 then "let rec " else "and ");
	Format.fprintf fmt "_sam%i _e _m newToken=function\n" i;
	Tmap.iter (fun t a -> pp_action fmt startR i t a ruleMap tokenTypeMap) actionT;
	let l = Tmap.fold (fun str _ l-> str::l) actionT (Ntmap.fold (fun str _ l -> str::l) goto []) in
	Format.fprintf fmt "\t|_->samFail [%a]\n" afficheListStr l

(* affiche une transition du goto *)
let pp_goto fmt i t target =
	Format.fprintf fmt "|%i,\"%s\"->_sam%i\n" i t target

let pp_mainGoto fmt i t target =
	Format.fprintf fmt "|%i, %s _ -> %i\n" i (String.uppercase_ascii t) target

(* affiche toutes les transitions du goto à partir d'un état *)
let pp_gotoStates fmt i gotoT ruleMap = 
	Ntmap.iter (pp_goto fmt i) gotoT

let pp_gotoStates2 fmt i gotoT ruleMap = 
	Ntmap.iter (pp_mainGoto fmt i) gotoT

(* affiche tout le programme *)
let pp_buildProg fmt program = 
	pp_header fmt program.head;
	Format.fprintf fmt "\n\n%a\n%a\n" pp_tokenDecl program.tokenList pp_declarationTypes program;
	let tokenTypeMap = List.fold_left (fun m (t,dT) -> if dT = None then m else Tmap.add t dT m) Tmap.empty program.tokenList in
	let rMap = List.fold_left (fun m (nm,tipe, rawProd, opt, cons) -> Rmap.add (nm,unRawProd rawProd, opt) (rawProd,cons) m) Rmap.empty program.gR.raw_rules in
	Imap.iter (fun i act -> if Tmap.cardinal act > 0 then pp_actionStates fmt program.gR.startR i act rMap tokenTypeMap (if Imap.mem i program.gotoTab then Imap.find i program.gotoTab else Ntmap.empty)) program.actionTab;
	Format.fprintf fmt "and goto i readRule = match i,readRule with\n";
	Imap.iter (fun i got ->if Ntmap.cardinal got > 0 then pp_gotoStates fmt i got rMap) program.gotoTab;
	Format.fprintf fmt "|_,_-> assert false\n\n%a\n" (pp_end program.gR.startR) program.startLTable;
	Format.pp_print_flush fmt ()
;;

let pp_main fmt program = 
	pp_header fmt program.head;
	Format.fprintf fmt "\n\n%a\n%a\n" pp_tokenDecl program.tokenList pp_declarationTypes program;
	let tokenTypeMap = List.fold_left (fun m (t,dT) -> if dT = None then m else Tmap.add t dT m) Tmap.empty program.tokenList in
	let rMap = List.fold_left (fun m (nm,tipe, rawProd, opt, cons) -> Rmap.add (nm,unRawProd rawProd, opt) (rawProd,cons) m) Rmap.empty program.gR.raw_rules in
	pp_action_table fmt program.actionTab rMap tokenTypeMap;
	pp_reduce_table fmt tokenTypeMap;
	Format.fprintf fmt "and goto i readRule = match List.hd i,List.hd readRule with\n";
	Imap.iter (fun i got ->if Ntmap.cardinal got > 0 then pp_gotoStates2 fmt i got rMap) program.gotoTab;
	Format.fprintf fmt "|_, %s t when 1 = List.length readRule -> raise (Output t)\n" (String.uppercase_ascii program.gR.startR);
	Format.fprintf fmt "|_,_-> assert false;;\n\n";
	pp_mainProgram fmt;
	pp_mainEnd fmt program.gR.startR program.startLTable;
	Format.pp_print_flush fmt ()
;;

(* affiche le fichier .mli*)
let pp_mli fmt program =
	pp_tokenDecl fmt program.tokenList;
	Format.fprintf fmt "\nexception Samenhir_Parsing_Error of string list\n";
	Format.fprintf fmt "val %s: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (%s)\n" program.gR.startR (findType program.gR.startR program.gR.raw_rules)
;;

