open Astype
open AstcompilN
open X86_64

let nTypeUndef = -1
let nTypeNothing = 0
let nTypeInt = 1
let nTypeFloat = 2
let nTypeBool = 3
let nTypeString = 4
let nTypeStruct = 5
(* À partir de 5 : structs *)
let nTypeArray = 65536 (* un array d'int à n dimension aura comme type : nTypeInt + n * nTypeArray *)


let valTrue = -1
let valFalse = 0

let popn n = addq (imm n) !%rsp


(* Compteur pour les étiquettes *)
let compteurFor = ref 0
let compteurWhile = ref 0
let compteurIf = ref 0
let compteurFunc = ref 0

let compteurString = ref 0
let compteurFloat = ref 0

(* These are for analytics *)
let compteurCall = ref 10
let compteurMalloc = ref 0

let sMap = Hashtbl.create 8
let fMap = Hashtbl.create 8

let getFor () =
	let temp = !compteurFor in
	compteurFor := !compteurFor + 1;
	("for_" ^ (string_of_int temp))
;;
let getWhile () =
	let temp = !compteurWhile in
	compteurWhile := !compteurWhile + 1;
	("while_" ^ (string_of_int temp))
;;
let getIf () =
	let temp = !compteurIf in
	compteurIf := !compteurIf + 1;
	("if_" ^ (string_of_int temp))
;;
let getFunc () =
	let temp = !compteurFunc in
	compteurFunc := !compteurFunc + 1;
	("func_" ^ (string_of_int temp))
;;


let exitLabel = "exit"

module Smap = Map.Make(String)

let structMap = ref (Smap.empty: Astype.structEnv)
let functionMap = ref (Smap.empty: Astype.funcMap)

let numStruct ident = nTypeStruct + snd (Smap.find ident !structMap)

type local_env = int Smap.t

let rec calcArb s = function
	| [] -> Failure
	| ([],i,j)::tl -> begin
		let (l, mini) = List.fold_left (fun (l, m) (_, i2, j2) -> if j2 < m then ([i2], j2) else if j2 > m then (l, m) else (i2::l, m)) ([i],j) tl in
		match l with
			| [] -> assert false
			| [i] -> Feuille (s, i)
			|_ -> Failure
		end
	|l -> let tM1 = List.fold_left (fun t (l, i, j) -> match l with
				| [] -> assert false
				| hd::tl ->
					let j = j + (if hd=Any then 1 else 0) in
					if TypeMap.mem hd t then
						let l1 = try TypeMap.find hd t with Not_found -> failwith "not found 1" in TypeMap.add hd ((tl, i, j)::l1) t
					else TypeMap.add hd [tl,i, j] t) TypeMap.empty l in
		let tM2 = if TypeMap.mem Any tM1 then
				let l = try TypeMap.find Any tM1 with Not_found -> failwith "not found 2" in
				TypeMap.mapi (fun k l2 -> if k = Any then l2 else l@l2) tM1
			else tM1
		in
		let tM3 = TypeMap.map (calcArb s) tM2 in
		Appels tM3


let rec alloc_expr (env: local_env) (offset:int):Astype.exprTyper -> (AstcompilN.expression * int) = function
	| EntierE i -> Entier i, offset
	| FlottantE f ->
		(* Tout comme les strings, les immediats flottants doivent être mis dans .data *)
		begin
		let s = string_of_float f in
		if not (Hashtbl.mem fMap s)
		then
			(Hashtbl.add fMap s !compteurFloat;
			compteurFloat := !compteurFloat + 1);
		Flottant f, offset
		end
	| ChaineE s ->
		begin
		if not (Hashtbl.mem sMap s)
		then
			(Hashtbl.add sMap s !compteurString;
			compteurString := !compteurString + 1);
		Chaine s, offset
		end
	| TrueE -> True, offset
	| FalseE -> False, offset
	| BlocE (_, eL) ->
		let (offset, eL) =
			List.fold_right
				(fun (_, e) (o1, l) -> let (e1, o2) = alloc_expr env offset e in (min o1 o2, e1::l))
				eL
				(offset, [])
		in Bloc eL, offset
	| CallE ((ident, iSet:(string * ISet.t)), eL) -> begin
		compteurCall := !compteurCall + 1;
		let (offset, eL) =
			List.fold_right
				(fun (_, e) (o1, l) -> let (e1, o2) = alloc_expr env offset e in (min o1 o2, e1::l))
				eL
				(offset, [])
		in
		match ident with 
		| "print" | "div" | "_getelement" | "_setelement" | "newarray" -> Call (ident, Feuille (ident, 0), eL), offset
		| _ ->
			let f = try Tmap.find ident !functionMap with Not_found -> failwith ("not found "^ident) in
			let arb:AstcompilN.functArbr = calcArb ident (ISet.fold (fun i l -> match Imap.find i f with
					|StructBuilder l1 |Funct (l1, _, _, _)  -> (List.fold_right (fun (_, p) l2 -> p::l2) l1 [],i,0)::l) iSet [])
			in Call (ident, arb, eL), offset
		end
	| NotE (_,e) -> let (e, o) = alloc_expr env offset e in Not e, o
	| MinusE (_, e) -> let (e, o) = alloc_expr env offset e in Minus e, o
	| BinopE (o, (_, e1), (_, e2)) ->
		let (e1, o1) = alloc_expr env offset e1 in
		let (e2, o2) = alloc_expr env offset e2 in
		Binop (o, e1, e2), min o1 o2
	| LvalueE l -> begin
		match l with
			| IdentL (_, ident, true) -> Ident (Dec (try Tmap.find ident env with Not_found -> failwith ("not found 3 :"^ident))), offset
			| IdentL (_, ident, false) -> Ident (Tag ident), offset
			| IndexL ((_, e), nameS, nameC) ->
				let (e, offset) = alloc_expr env offset e in
				let map = fst (Smap.find nameS !structMap) in
				let i2 = 16 * fst (Smap.find nameC map) in
				Index (e, nameS, i2), offset
		end
	| LvalueAffectE (l, (_, e)) -> begin
		let e, offset = alloc_expr env offset e in
		match l with
			| IdentL (_, ident, false) -> LvalueAffectV ((Tag ident), e), offset
			| IdentL (_, ident, true) -> LvalueAffectV (Dec (Tmap.find ident env), e), offset
			| IndexL ((_, e2), nameS, nameC) ->
				let (e2, o2) = alloc_expr env offset e2 in
				let map = fst (Smap.find nameS !structMap) in
				let i2 = 16 * fst (Smap.find nameC map) in
				LvalueAffectI (e2, nameS, i2, e), min o2 offset
		end
	| ReturnE (p, None) -> Ret (p, Nothing), offset (* À vérifier *)
	| ReturnE (p, Some (_, e)) ->
		let (e, offset) = alloc_expr env offset e in
		Ret (p, e), offset
	| ForE (i, tmap, (_, e1), (_, e2), (_, eL)) ->
		let (e1, o1) = alloc_expr env offset e1 in
		let (e2, o2) = alloc_expr env offset e2 in
		let (env2, fpcur2) = Tmap.fold (fun k _ (m, n) -> if k!= i then (Tmap.add k (n-16) m, n-16) else (m, n)) tmap (env, offset - 16) in
		let env = Tmap.add i (offset-16) (if Tmap.mem i env2  then Tmap.remove i env2 else env2) in
		let (l,o3) = List.fold_right (fun (_, e) (l, o1) -> let e,o2 = (alloc_expr env fpcur2 e) in (e::l, min o1 o2)) eL ([], min fpcur2 (min o1 o2)) in
		For (offset-16, fpcur2, e1, e2, l), o3
	| WhileE ((_, e), tmap, (_, eL)) ->
		let (e, o1) = alloc_expr env offset e in
		let env2, fpcur2 = Tmap.fold (fun k _ (m, n) -> (Tmap.add k (n-16) m, n-16)) tmap (env, offset) in
		let (l,o2) = List.fold_right (fun (_, e) (l, o1) -> let e,o2 = (alloc_expr env2 fpcur2 e) in (e::l, min o1 o2)) eL ([], min fpcur2 o1) in
		While (e, offset, fpcur2, l), o2
	| IfE ((_, e), (_, eL), els) ->
		let (e, o1) = alloc_expr env offset e in
		let (l, o2) = List.fold_right (fun (_, e) (l, o1) -> let e,o2 = (alloc_expr env offset e) in (e::l, min o1 o2)) eL ([], o1) in
		let (els, o3) = alloc_else env offset els in
		If (e, l, els), min o2 o3
and alloc_else (env:local_env) (offset:int) = function
	| EndI -> End, offset
	| ElseI (_, eL) ->
		let (l, o) = List.fold_right (fun (_, e) (l, o1) -> let e,o2 = (alloc_expr env offset e) in (e::l, min o1 o2)) eL ([], offset)
		in Else l, o
	| ElseifI ((_, e), (_, eL), els) ->
		let (e, o1) = alloc_expr env offset e in
		let (l, o2) = List.fold_right (fun (_, e) (l, o1) -> let e,o2 = (alloc_expr env offset e) in (e::l, min o1 o2)) eL ([], o1) in
		let (els, o3) = alloc_else env offset els in
		Elseif (e, l, els), min o3 o2

let alloc_fichier (eL, varMap, sEnv, fMap:fichierTyper):fichier =
	structMap := sEnv;
	functionMap := fMap;
	let (l, o) = List.fold_right (fun e (l, o) -> let (e, o2) = alloc_expr Tmap.empty 0 e in (e::l, min o o2)) eL ([], 0) in
	(l, o, varMap, fMap)

let pushn n =
	let c = ref nop in
	for i = 1 to n/16 do
		c := !c ++ pushq (imm nTypeUndef) ++ pushq !%rax (* Pas plutôt !%rbx? *)
	done;
	!c

let compteurArbreAppel = ref 0

let int_of_type t = match t with
  | Any -> nTypeUndef
  | Nothing -> nTypeNothing
  | Int64 -> nTypeInt
  | Float64 -> nTypeFloat
  | Bool -> nTypeBool
	| String -> nTypeString
	| Array -> nTypeArray
  | S s -> numStruct s

let newFlagArb () =
	let t = !compteurArbreAppel in
	compteurArbreAppel := 1 +t;
	"jmp"^string_of_int t

let rec buildArb (p:int) (f:string) (l:string):functArbr -> [`text] asm = function
	| Failure -> (label l ++ jmp exitLabel)
	| Feuille (s,i) -> compteurCall := !compteurCall + 1;
		label l ++ call (s^"_"^string_of_int i) ++ jmp f
	| Appels tM ->
		if TypeMap.cardinal tM == 1 then
			if TypeMap.mem Any tM then buildArb (p-1) f l (TypeMap.find Any tM)
			else
				let (t,arb) = TypeMap.choose tM in
				if (int_of_type t = nTypeArray) then 
					label l ++ cmpq (imm (int_of_type t)) (ind ~ofs:(16*p - 8) rsp) ++ jg exitLabel ++ buildArb (p-1) f (newFlagArb ()) arb
				else label l ++ cmpq (imm (int_of_type t)) (ind ~ofs:(16*p - 8) rsp) ++ jne exitLabel ++ buildArb (p-1) f (newFlagArb ()) arb
		else
			let (c1,l1) = TypeMap.fold (fun k a (c,l) -> if k=Any then (c,l)
								else let dir = newFlagArb () in
								if (int_of_type k) = nTypeArray then 
									(c ++ cmpq (imm (int_of_type k)) (ind ~ofs:(16*p - 8) rsp) ++ jge dir, (dir,a)::l)
								else (c ++ cmpq (imm (int_of_type k)) (ind ~ofs:(16*p - 8) rsp) ++ je dir, (dir,a)::l)
							) tM (label l, []) in
			let c = if TypeMap.mem Any tM then
					buildArb (p-1) f (newFlagArb ()) (TypeMap.find Any tM)
				else
					jmp exitLabel in
			let c2 = List.fold_left (fun c (dir,a) -> c ++ buildArb (p-1) f dir a) (c1 ++ c) l1 in
			c2


let rec compile_expr = function
	| Entier i -> pushq (imm nTypeInt) ++ movq (imm64 i) !%rax ++ pushq !%rax
	| Flottant f -> (*pushq (imm nTypeFloat) ++ pushq (immD f)*) (* À changer *)
		let n = string_of_int (Hashtbl.find fMap (string_of_float f)) in
		pushq (imm nTypeFloat) ++ pushq ((if estMac then lab else ilab) ("float_"^n))
	| Chaine s ->
		let n = string_of_int (Hashtbl.find sMap s) in
		pushq (imm nTypeString) ++ (if estMac then (fun x -> leaq x rax ++ pushq !%rax) else pushq) (lab ("string_"^n))
	| True -> pushq (imm nTypeBool) ++ pushq (imm valTrue)
	| False -> pushq (imm nTypeBool) ++ pushq (imm valFalse)
	| Nothing -> pushq (imm nTypeNothing) ++ pushq (imm 0)
	| Bloc bloc -> compile_bloc bloc
(*	| EntierIdent (entier, label) -> compile_expr (Binop (Times, Entier entier, Ident label))
	| EntierParG (entier, bloc) -> compile_expr (Binop (Times, Entier entier, Bloc bloc))
	| ParDIdent (exp, label) -> compile_expr (Binop (Times, exp, Ident label))*)
	| Call (ident, funcArbr, expList) ->
		compteurCall := !compteurCall + 1;
		let rec parcours liste =
			match liste with
			| [] -> nop
			| t :: q ->
				let eq = parcours q in
				let e = compile_expr t in
			e ++ eq
		in
		let e = parcours expList in
		if ident = "print" then
			begin
			e ++ (movq (imm (List.length expList)) !%rsi) ++ call "print_0" ++
			popn (16 * List.length expList) ++ (pushq (imm nTypeNothing)) ++ pushq !%rbx
			end
		else if ident = "div" then
			e ++ popq rcx ++ popq rdx ++ popq rbx ++ popq rax ++
					(cmpq (imm nTypeInt) !%rax) ++ (jne exitLabel) ++
	  				(cmpq (imm nTypeInt) !%rdx) ++ (jne exitLabel) ++
	  				movq !%rbx !%rax ++ xorq !%rdx !%rdx ++ (cmpq (imm 0) !%rax) ++ (movq (imm (-1)) !%r13) ++ (cmovs !%r13 rdx) ++
						(cmpq (imm 0) !%rcx) ++ (je exitLabel) ++
	  				idivq !%rcx ++ pushq (imm nTypeInt) ++ pushq !%rax
		else
			begin
				let flagfin = newFlagArb () in
				let flagdeb = newFlagArb () in
				e ++ (buildArb (List.length expList) flagfin flagdeb funcArbr) ++
				label flagfin ++
				popn (16 * List.length expList) ++ (pushq !%rax) ++ pushq !%rbx
			end
	| Not expr ->
		compile_expr expr ++ (popq rbx) ++ (popq rax) ++
		(cmpq (imm nTypeBool) !%rax) ++ (jne exitLabel) ++ (* Commande pour exit en cas d'erreur!*)
		(pushq (imm nTypeBool)) ++ (notq !%rbx) ++ (pushq !%rbx)
	| Minus expr -> compile_expr (Binop (Minus, Entier (Int64.of_int 0), expr))
	| Binop (op, e1, e2) ->
		let ins1 = compile_expr e1 in
		let ins2 = compile_expr e2 in
		let (label1, label2) = getIf (), getIf () in
		let depilation = (popq rbx) ++ (popq rax) ++ (popq rdx) ++ (popq rcx) in
		let deb = ins1 ++ ins2 ++ depilation in
		let operation =
		match op with
		| Eq -> deb ++ (pushq (imm nTypeBool)) ++ (cmpq !%rax !%rcx) ++ (jne label1) ++
							 (cmpq !%rbx !%rdx) ++ (jne label1) ++ (pushq (imm valTrue)) ++
							 								   (jmp label2) ++ (label label1) ++ (pushq (imm valFalse)) ++ (label label2)
	  | Neq -> deb ++ (pushq (imm nTypeBool)) ++ (cmpq !%rax !%rcx) ++ (jne label1) ++
							  (cmpq !%rbx !%rdx) ++ (jne label1) ++ (pushq (imm valFalse)) ++
							 								   (jmp label2) ++ (label label1) ++ (pushq (imm valTrue)) ++ (label label2)
	  | Lo -> deb ++ (cmpq (imm nTypeInt) !%rax) ++ (jne exitLabel) ++
							  (cmpq (imm nTypeInt) !%rcx) ++ (jne exitLabel) ++
							  (pushq (imm nTypeBool)) ++
							  (cmpq !%rbx !%rdx) ++ (jge label1) ++ (pushq (imm valTrue)) ++
							 								   (jmp label2) ++ (label label1) ++ (pushq (imm valFalse)) ++ (label label2)
	  | Gr -> deb ++ (cmpq (imm nTypeInt) !%rax) ++ (jne exitLabel) ++
							  (cmpq (imm nTypeInt) !%rcx) ++ (jne exitLabel) ++
							  (pushq (imm nTypeBool)) ++
							  (cmpq !%rbx !%rdx) ++ (jle label1) ++ (pushq (imm valTrue)) ++
							 								   (jmp label2) ++ (label label1) ++ (pushq (imm valFalse)) ++ (label label2)
	  | Leq -> deb ++ (cmpq (imm nTypeInt) !%rax) ++ (jne exitLabel) ++
							   (cmpq (imm nTypeInt) !%rcx) ++ (jne exitLabel) ++
							   (pushq (imm nTypeBool)) ++
							   (cmpq !%rbx !%rdx) ++ (jg label1) ++ (pushq (imm valTrue)) ++
							 								   (jmp label2) ++ (label label1) ++ (pushq (imm valFalse)) ++ (label label2)
	  | Geq -> deb ++ (cmpq (imm nTypeInt) !%rax) ++ (jne exitLabel) ++
							   (cmpq (imm nTypeInt) !%rcx) ++ (jne exitLabel) ++
							   (pushq (imm nTypeBool)) ++
							   (cmpq !%rbx !%rdx) ++ (jl label1) ++ (pushq (imm valTrue)) ++
							 								   (jmp label2) ++ (label label1) ++ (pushq (imm valFalse)) ++ (label label2)
	  | Plus -> deb ++ (cmpq (imm nTypeInt) !%rax) ++ (jne exitLabel) ++
							    (cmpq (imm nTypeInt) !%rcx) ++ (jne exitLabel) ++
							    (pushq (imm nTypeInt)) ++
							    (addq !%rdx !%rbx) ++ (pushq !%rbx)
	  | Minus -> deb ++ (cmpq (imm nTypeInt) !%rax) ++ (jne exitLabel) ++
							    (cmpq (imm nTypeInt) !%rcx) ++ (jne exitLabel) ++
							    (pushq (imm nTypeInt)) ++
							    (subq !%rbx !%rdx) ++ (pushq !%rdx)
	  | Times -> deb ++ (cmpq (imm nTypeInt) !%rax) ++ (jne exitLabel) ++
							    (cmpq (imm nTypeInt) !%rcx) ++ (jne exitLabel) ++
							    (pushq (imm nTypeInt)) ++
							    (imulq !%rdx !%rbx) ++ (pushq !%rbx)
	  | Modulo -> deb ++ (cmpq (imm nTypeInt) !%rax) ++ (jne exitLabel) ++
	  				(cmpq (imm nTypeInt) !%rcx) ++ (jne exitLabel) ++
	  				movq !%rdx !%rax ++ movq !%rbx !%rcx ++ xorq !%rdx !%rdx ++
	  				(cmpq (imm 0) !%rax) ++ (movq (imm (-1)) !%r13) ++ (cmovs !%r13 rdx) ++
	  				idivq !%rcx ++ pushq (imm nTypeInt) ++ pushq !%rdx
	  | Exp -> deb ++ (cmpq (imm nTypeInt) !%rax) ++ (jne exitLabel) ++
	  				(cmpq (imm nTypeInt) !%rcx) ++ (jne exitLabel) ++
						(movq !%rdx !%rax) ++ (movq !%rbx !%rbx) ++ (movq (imm 1) !%rcx) ++
						(label label1) ++
						(cmpq (imm 0) !%rbx) ++ (je label2) ++
						(decq !%rbx) ++ (imulq !%rax !%rcx) ++
						(jmp label1) ++
						(label label2) ++
						(movq (imm nTypeInt) !%rax) ++ (movq !%rcx !%rbx) ++
						(pushq !%rax) ++ (pushq !%rbx)
	  | And -> ins1 ++ popq rbx ++ popq rax ++ (cmpq (imm nTypeBool) !%rax) ++ jne exitLabel ++
	  				(cmpq (imm valTrue) !%rbx) ++ jne label1 ++
	  				ins2 ++ popq rbx ++ popq rax ++ (cmpq (imm nTypeBool) !%rax) ++ jne exitLabel ++
	  				pushq (imm nTypeBool) ++ pushq !%rbx ++ jmp  label2 ++ (label label1) ++ pushq (imm nTypeBool) ++ pushq (imm valFalse)
	  				++ (label label2)
	  | Or -> ins1 ++ popq rbx ++ popq rax ++ (cmpq (imm nTypeBool) !%rax) ++ jne exitLabel ++
	  				(cmpq (imm valTrue) !%rbx) ++ je label1 ++
	  				ins2 ++ popq rbx ++ popq rax ++ (cmpq (imm nTypeBool) !%rax) ++ jne exitLabel ++
	  				pushq (imm nTypeBool) ++ pushq !%rbx ++ jmp  label2 ++ (label label1) ++ pushq (imm nTypeBool) ++ pushq (imm valTrue)
	  				++ (label label2)
		in operation
	| Ident (Tag name) ->
		movq ((if estMac then lab else ilab) (name^"_type")) !%rax ++ cmpq (imm nTypeUndef) !%rax ++
		je exitLabel ++ movq ((if estMac then lab else ilab) (name^"_val")) !%rbx ++
		pushq !%rax ++ pushq !%rbx
	| Ident (Dec offset) ->
		movq (ind ~ofs:(offset+8) rbp) !%rax ++ cmpq (imm nTypeUndef) !%rax ++
		je exitLabel ++ pushq !%rax ++ pushq (ind ~ofs:offset rbp)
	| Index (exp, ident, offset) ->
		print_string ident;
		print_int offset;
		print_newline ();
		let numClasse = numStruct ident in
		(compile_expr exp) ++ (popq rbx) ++ (popq rax) ++ (cmpq (imm numClasse) !%rax) ++ (jne exitLabel) ++
		(movq (ind ~ofs:(offset + 0) rbx) !%rax) ++ (movq (ind ~ofs:(offset + 8) rbx) !%rbx) ++ pushq !%rax ++ pushq !%rbx
	| LvalueAffectV (Tag name, expr) ->
		let code = compile_expr expr in
		code ++ (popq rbx) ++ (popq rax) ++ movq !%rbx (univerlab (name^"_val")) ++ movq !%rax (univerlab (name^"_type")) ++ pushq !%rax ++ pushq !%rbx
	| LvalueAffectV (Dec offset, expr) ->
		let code = compile_expr expr in
		code ++ (popq rbx) ++ (popq rax) ++ (movq !%rax (ind ~ofs:(offset+8) rbp)) ++ (movq !%rbx (ind ~ofs:offset rbp)) ++ pushq !%rbx ++ pushq !%rax
	| LvalueAffectI (exp1, ident, entier, exp2) ->
		let code1 = compile_expr exp2 in (* J'ai changé code1 et code2 :D *)
		let code2 = compile_expr exp1 in
		let (field_map, numero) = Tmap.find ident !structMap in (* numero est le code de type de la structure - 5*)
		let numeroBis = numero + nTypeStruct in
		let (cle, (field_index, field_type)) = Tmap.find_first (fun cle -> let (num, _) = Tmap.find cle field_map in num = numero) field_map in
		let comparaison = (popq r14) ++ (popq rax) ++ (cmpq (imm numeroBis) !%rax) ++ (jne exitLabel) in
		let target_type = (popq rbx) ++ (popq rax) ++
			(if field_type != Any then (cmpq (ind ~ofs:(entier + 0) r14) !%rax) ++ (jne exitLabel) else nop) ++(* vérification de type qu'on met dans le champ *)
			(movq !%rax (ind ~ofs:(entier + 0) r14)) ++
			(movq !%rbx (ind ~ofs:(entier + 8) r14)) in
		code1 ++ code2 ++ comparaison ++ target_type ++ pushq !%rax ++ pushq !%rbx
	| Ret (pjtype, exp) ->
		compile_expr exp ++ (popq rbx) ++ (popq rax) ++
		(if pjtype = Any then nop else (cmpq (imm (int_of_type pjtype)) !%rax ++
		jne exitLabel)) ++ movq !%rbp !%rsp ++ popq rbp ++ ret
	| For (posC, posFLoc, exp1, exp2, bloc) ->
		let lDeb = getFor () in
		let lFin = getFor () in
		let e1 = compile_expr exp1 in
		let e2 = compile_expr exp2 in
		let b = compile_bloc bloc in
		e1 ++ e2 ++ movq (imm nTypeInt) (ind ~ofs:(posC+8) rbp) ++
		(popq rdx) ++ (popq rcx) ++ (popq rbx) ++ (popq rax) ++
		cmpq (imm nTypeInt) !%rcx ++ jne exitLabel ++
		cmpq (imm nTypeInt) !%rax ++ jne exitLabel ++
		decq !%rbx ++
		pushq !%rbx ++ pushq !%rdx ++
		jmp lFin ++
		label lDeb ++
		b ++
		popn 16 ++
		label lFin ++
		popq rcx ++ popq rax ++
		incq !%rax ++ cmpq !%rax !%rcx ++
		pushq !%rax ++ pushq !%rcx ++ (movq !%rax (ind ~ofs:posC rbp)) ++
		jge lDeb ++
		popn 16 ++
		pushq (imm nTypeNothing) ++ pushq !%rax
	| While (exp, debLoc, finLoc, bloc) ->
		let e = compile_expr exp in
		let b = compile_bloc bloc in
		let (label1, label2) = (getWhile (), getWhile ()) in
		let comp = (label label1) ++ e ++ (popq rbx) ++ (popq rax) ++ (cmpq (imm (nTypeBool)) !%rax) ++ (jne exitLabel) ++ (cmpq (imm valTrue) !%rbx) ++ (jne label2) in
		let corps = b ++ popn 16 ++ (jmp label1) ++ (label label2) in
		comp ++ corps ++ pushq (imm nTypeNothing) ++ pushq !%rax
	| If (exp, bloc, else_) ->
		let c = compile_expr exp in
		let c1 = compile_bloc bloc in
		let c2 = compile_else_ else_ in
		let (label1, label2) = (getIf (), getIf ()) in
		c ++ (popq rbx) ++ (popq rax) ++ (cmpq (imm nTypeBool) !%rax) ++ (jne exitLabel) ++
		(cmpq (imm valFalse) !%rbx) ++ (je label1) ++ c1 ++ (jmp label2) ++ (label label1) ++ c2 ++ (label label2)
and compile_else_ = function
	| End -> pushq (imm nTypeNothing) ++ pushq !%rax
	| Else bloc -> compile_bloc bloc
	| Elseif (exp, bloc, else_) -> compile_expr (If (exp, bloc, else_))
and compile_bloc = function
	| [] -> pushq (imm nTypeNothing) ++ pushq !%rax
	| [t] -> compile_expr t
	| t :: q -> (compile_expr t) ++ popn 16 ++ (compile_bloc q)


let compile_function f e fpmax =
	let code =
		label f ++
		pushq !%rbp ++
		movq !%rsp !%rbp ++ pushn fpmax ++
		compile_expr e ++ popq rax ++
		popn fpmax ++ popq rbp ++ ret
	in
	code


let compile_fun (n:string) (i:int) = function
  | Funct (argL, tmap, (_, eL), rT) -> (
  	let (_, env) = List.fold_right (fun (i, _) (n,t) -> (n + 16, Tmap.add i n t)) argL (16, Tmap.empty) in
		let env2, fpcur2 = Tmap.fold (fun k _ (m, n) -> if Tmap.mem k env then (m,n) else (Tmap.add k (n-16) m, n-16)) tmap (env, 0) in
		let (eL,o2) = List.fold_right (fun (_, e) (l, o1) -> let e,o2 = (alloc_expr env2 fpcur2 e) in (e::l, min o1 o2)) eL ([], fpcur2) in
  	let code = List.fold_left (fun c e -> c ++ compile_expr e ++ popq rbx ++ popq rax) nop eL in
    label (n^"_"^string_of_int i)++
    pushq !%rbp ++ movq !%rsp !%rbp ++
    pushn (-o2) ++
    code ++
    (if rT = Any then nop else (cmpq (imm (int_of_type rT)) !%rax ++ jne exitLabel)) ++
    movq !%rbp !%rsp ++ popq rbp ++
    ret)
	| StructBuilder eL ->
		compteurMalloc := !compteurMalloc + 1;
		compteurCall := !compteurCall + 1;
    let nType = numStruct n in
    let longueur= List.length eL in
    let code = ref nop in
    for i = 1 to longueur do
      code := !code ++ (* Penser à ajouter un check de type si le type du champ est != Any *)
        movq (ind ~ofs:(8+i*16) rsp) !%r11 ++ movq !%r11 (ind ~ofs:(16*(longueur-i)) rax) ++
        movq (ind ~ofs:(i*16) rsp) !%r11 ++ movq !%r11 (ind ~ofs:(16*(longueur-i)+8) rax)
    done;
    label (n^"_"^string_of_int i) ++
    pushq !%rbp ++ movq !%rsp !%rbp ++
		movq (imm (16*longueur)) !%rdi ++ call "malloc" ++ !code ++
    movq !%rax !%rbx ++ movq (imm nType) !%rax ++
    movq !%rbp !%rsp ++ popq rbp ++
    ret

let compile_program f ofile =
 let (eL, i, smap, fmap) = alloc_fichier f in (* smap est le map des variables globales *)
 print_int i;
 print_newline ();
 let code = List.fold_left (fun d e -> (if d!=nop then d ++ popn 16 else nop) ++ compile_expr e) nop eL in
 let codefun = Tmap.fold (fun k imap asm -> Imap.fold (fun i f asm2 -> asm2 ++ compile_fun k i f) imap asm) fmap nop in
 let deplq = if estMac then (fun x -> leaq x rdi) else (fun x -> movq x !%rdi) in
 let p =
   { text =
		globl "main" ++ label "main" ++
		pushq !%rbx ++ pushq !%r12 ++
		pushq !%rbp ++ movq !%rsp !%r12 ++
		movq !%rsp !%rbp ++
		pushn (-i) ++
		code ++
		movq !%rbp !%rsp ++
		popq rbp ++
		popq r12 ++ popq rbx ++
		movq (imm 0) !%rax ++ (* exit *)
		ret ++

		label "div" ++ (* Fonction de division entière *)
    movq (ind ~ofs:8 rsp) !%rcx ++
    movq (ind ~ofs:24 rsp) !%rax ++
    xorq !%rdx !%rdx ++
    idivq !%rax ++
		ret ++

		label "newarray_0" ++
		popq rdx ++ popq rcx ++ (* valeur d'initialisation *)
		popq rbx ++ popq rax ++ (* longueur de l'array *)
		cmpq (imm nTypeInt) !%rax ++ jne (label exitLabel) ++ (* on vérifie que l'indice est bien entier *)
		addq (imm 1) !%rbx ++ movq !%rbx !%rax ++ imulq (imm 8) !%rax ++ (* On ajouter à la taille les 2 mots : type/taille *)
		movq !%rax !%rdi ++ 
		call "malloc" ++ (* /!\ pas d'initialisation pour l'instant *)
		(* L'adresse du début de l'array est en %rax *)
		movq !%rcx (ind ~ofs:0 rax) ++ (* On stocke le type *)
		movq !%rbx (ind ~ofs:8 rax) ++ (* On stocke la taille *)
		addq (imm nTypeArray) !%rcx ++ (* On calcule le nouveau type (ajout de nTypeArray) *)
		pushq !%rcx ++ pushq !%rax ++ (* on empile le type et le pointeur vers le array *)
		movq !%rax !%rbx ++ movq !%rcx !%rax ++ (* Et on met ça sur %rax-%rbx *)
		ret ++


		label "_getelement_0" ++
		popq rdx ++ popq rcx ++ (* type et valeur de l'indice *)
		popq rbx ++ popq rax ++ (* type et valeur du pointeur vers le début *)
		comp (nTypeArray) !%rax ++ jg exitLabel ++ (* On vérifie que c'est bien un array *)
		cmpq (nTypeInt) !%rcx ++ jne exitLabel ++ (* On vérifie que l'indice est bien un entier *)
		cmpq (ind ~ofs:8 rbx) !%rdx ++ jle exitLabel ++ (* On compare qu'on reste dans les bornes de l'array ;) *) (* TODO vérifier que c'est bon *)
		imulq (imm 1) !%rbx ++
		movq (ind ~ofs:0 rbx) !%rax ++ (* Acquisition du type *)
		movq (ind ~ofs:16 ~index:rdx ~scale:8 rbx) !%rbx ++ (* Acquisition de la valeur *)
		pushq !%rax ++ push !%rbx ++ (* On empile le résultat *)
		ret ++

		label "_setelement_0" ++
		popq r13 ++ popq r12 ++ (* type et valeur à insérer*)
		popq rdx ++ popq rcx ++ (* type et valeur de l'indice *)
		popq rbx ++ popq rax ++ (* type et valeur de la donnée (array, a priori) à affecter *)
		comp (nTypeArray) !%rax ++ jg exitLabel ++ (* On vérifie que c'est bien un array *)
		cmpq (nTypeInt) !%rcx ++ jne exitLabel ++ (* On vérifie que l'indice est bien un entier *)
		cmpq (ind ~ofs:0 rbx) !%r12 ++ jne exitLabel ++ (* On vérifie que la valeur insérée a le même type que le reste *)
		cmpq (ind ~ofs:8 rbx) !%rdx ++ jle exitLabel ++ (* On compare qu'on reste dans les bornes de l'array ;) *)
		imulq (imm 1) !%rbx ++
		movq !%r13 (ind ~ofs:16 ~index:rdx ~scale:8 rbx) ++ (* Insertion de la valeur *)
		ret ++

		label "print_0" ++ (* Fonction principale print *)
		pushq !%rbp ++
		movq !%rsp !%rbp ++
		movq !%rsi !%r13 ++ (* Compteur d'arguments /!\ un seul mot!! *)
		label "print_loop" ++
		cmpq (imm 0) !%r13 ++
		je "print_exit" ++
		movq !%r13 !%r9 ++
		imulq (imm 2) !%r9 ++
		 (* Nouvel index *)
		movq (ind  ~index:r9 ~scale:8 rbp) !%rbx ++
		movq (ind ~ofs:(8) ~index:r9 ~scale:8 rbp) !%rax ++
		call "print_value" ++
		decq !%r13 ++
		jmp "print_loop" ++
		label "print_exit" ++
		movq !%rsp !%rbp ++
		popq rbp ++
		ret ++

		label "print_value" ++ (* Prend un argument et appelle la fonction print spécialisée en fonction de son type *)
		(movq !%rbx !%rdi) ++ (*Il doit y avoir le type dans rax et la valur dans rbx*)
		(cmpq (imm nTypeInt) !%rax) ++
		je "ifInt" ++
		(cmpq (imm nTypeFloat) !%rax) ++
		je "ifFloat" ++
		(cmpq (imm nTypeBool) !%rax) ++
		je "ifBool" ++
		(cmpq (imm nTypeString) !%rax) ++
		je "ifString" ++
		label "ifBool" ++
		call "print_bool" ++
		ret ++
		label "ifInt" ++
		call "print_int" ++
		ret ++
		label "ifFloat" ++
		call "print_float" ++
		ret ++
		label "ifString" ++
		call "print_string" ++
    ret ++

		label "print_int" ++
		movq !%rdi !%rsi ++
		deplq (lab ".Sprint_int") ++
		movq (imm 0) !%rax ++
		call "printf" ++
    ret ++

		label "print_float" ++
		movq !%rdi !%xmm0 ++
		deplq (lab ".Sprint_float") ++
		movq (imm 1) !%rax ++
		call "printf" ++
		movq (imm 0) !%rax ++
    ret ++

		label "print_string" ++
		movq (imm 0) !%rax ++
		call "printf" ++
    ret ++

		label "print_bool" ++
		movq !%rdi !%rsi ++
		cmpq (imm valFalse) !%rsi ++
		je "print_false" ++
		deplq (lab ".Sprint_true") ++
		jmp "print_end" ++
		label "print_false" ++
		deplq (lab ".Sprint_false") ++
		label "print_end" ++
		movq (imm 0) !%rax ++
		call "printf" ++
    ret ++

		label exitLabel ++
		deplq (lab ".Sprint_error") ++
		call "printf" ++
		movq !%r12 !%rsp ++
		popq rbp ++
		popq r12 ++ popq rbx ++
		movq (imm 1) !%rax ++
		ret ++
       codefun;
     data =
       Hashtbl.fold (fun x i l -> l ++ label ("string_"^string_of_int i) ++ string (Scanf.unescaped x)) sMap nop ++
			 Hashtbl.fold (fun x i l -> l ++ label ("float_"^string_of_int i) ++ (double (float_of_string x))) fMap nop ++
			 Tmap.fold (fun x i l -> if x<> "nothing" then l ++ label (x^"_type") ++ (dquad [nTypeUndef])
			 														++ label (x^"_val") ++ (dquad [0]) else l) smap nop ++
		(label "nothing_type" ++ (dquad [nTypeNothing])) ++
		(label "nothing_val" ++ (dquad [0])) ++
       (label ".Sprint_int" ++ string "%zd") ++
			 (label ".Sprint_float" ++ string "%f") ++
			 (label ".Sprint_string" ++ string "%s") ++
			 (label ".Sprint_endline" ++ string "\n") ++
			 (label ".Sprint_true" ++ string "true") ++
			 (label ".Sprint_false" ++ string "false") ++
			 (label ".Sprint_error" ++ string "type or \"Division by zero\" failure\n")
   }
 in
 let f = open_out ofile in
 let fmt = Format.formatter_of_out_channel f in
 X86_64.print_program fmt p;
 Format.fprintf fmt "@?";
 close_out f
;;

let get_analytics () =
	(!compteurFor, !compteurWhile, !compteurIf, !compteurFunc, !compteurString, !compteurFloat, !compteurCall, !compteurMalloc)


(*
label "print_int" ++
movq !%rdi !%rsi ++
movq (ilab ".Sprint_int") !%rdi ++
movq (imm 0) !%rax ++
call "printf" ++
ret
avec dans data : (label ".Sprint_int" ++ string "%d\n")
*)
