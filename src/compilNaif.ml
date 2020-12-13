open Astype
open AstcompilN
open X86_64

let nTypeUndef = -1
let nTypeNothing = 0
let nTypeInt = 1
let nTypeFloat = 2
let nTypeBool = 3
let nTypeString = 4
(* À partir de 5 : structs *)

module Smap = Map.Make(String)

let structMap = ref (Smap.empty: Astype.structEnv)

type local_env = (int * int) Smap.t

let calcPos (prof:int) (nm:string) (env:local_env) =
	let (p, dec) = Smap.find nm env in
	(prof - p, dec)

let rec alloc_expr (prof:int) (env: local_env):Astype.exprTyper -> AstcompilN.expression = function
	| EntierE i -> Entier i
	| FlottantE f -> Flottant f
	| ChaineE s -> Chaine s
	| TrueE -> True
	| FalseE -> False
	| EntierIdentE (i, _,  nm, true) -> EntierIdent (i, Dec (calcPos prof nm env))
	| EntierIdentE (i, _,  nm, false) -> EntierIdent (i, Tag nm)
	| EntierParGE  (i, (_, eL)) ->
		let l = List.fold_right
			(fun (_, e) l -> (alloc_expr prof env e)::l)
			eL
			[]
		in EntierParG (i, l)
	| BlocE (_, eL) ->
		Bloc (
			List.fold_right
				(fun (_, e) l -> (alloc_expr prof env e)::l)
				eL
				[])
	| ParDIdentE ((_, e), _, nm, true) ->
		ParDIdent (alloc_expr prof env e, Dec (calcPos prof nm env))
	| ParDIdentE ((_, e), _, nm, false) ->
		ParDIdent (alloc_expr prof env e, Tag nm)
	| CallE _ -> assert false
	| NotE (_,e) -> Not (alloc_expr prof env e)
	| MinusE (_, e) -> Minus (alloc_expr prof env e)
	| BinopE (o, (_, e1), (_, e2)) ->
		let e1 = alloc_expr prof env e1 in
		let e2 = alloc_expr prof env e2 in
		Binop (o, e1, e2)
	| LvalueE l -> begin
		match l with
			| IdentL (_, ident, true) -> Ident (Dec (calcPos prof ident env))
			| IdentL (_, ident, false) -> Ident (Tag ident)
			| IndexL ((_, e), nameS, nameC) ->
				let e = alloc_expr prof env e in
				let i2 = 0 in
				Index (e, nameS, i2)
		end
	| LvalueAffectE (l, (_, e)) -> begin
		let e = alloc_expr prof env e in
		match l with
			| IdentL (_, ident, false) -> LvalueAffectV ((Tag ident), e)
			| IdentL (_, ident, true) -> LvalueAffectV (Dec (calcPos prof ident env), e)
			| IndexL ((_, e2), nameS, nameC) ->
				let e2 = alloc_expr prof env e2 in
				let i2 = 0 in
				LvalueAffectI (e2, nameS, i2, e)
		end
	| ReturnE (p, None) -> Ret (p, Nothing)
	| ReturnE (p, Some (_, e)) ->
		let e = alloc_expr prof env e in
		Ret (p, e)
	| ForE (i, tmap, (_, e1), (_, e2), (_, eL)) ->
		let e1 = alloc_expr prof env e1 in
		let e2 = alloc_expr prof env e2 in
		let env2, fpcur2 = Tmap.fold (fun k _ (m, n) -> if k!= i then (Tmap.add k (prof+1, n-16) m, n-16) else (m, n)) Tmap.empty (env, -32) in
		let env = Tmap.add i (prof+1, -16) env2 in
		let l = List.fold_right (fun (_, e) l -> (alloc_expr (prof+1) env e)::l) eL [] in
		For (fpcur2, e1, e2, l)
	| WhileE ((_, e), tmap, (_, eL)) ->
		let e = alloc_expr prof env e in
		let env2, fpcur2 = Tmap.fold (fun k _ (m, n) -> (Tmap.add k (prof+1, n-16) m, n-16)) Tmap.empty (env, 0) in
		let l = List.fold_right (fun (_, e) l -> (alloc_expr (prof+1) env2 e)::l) eL [] in
		While (e, fpcur2, l)
	| IfE ((_, e), (_, eL), els) ->
		let e = alloc_expr prof env e in
		let l = List.fold_right (fun (_, e) l -> (alloc_expr prof env e)::l) eL [] in
		let els = alloc_else prof env els in
		If (e, l, els)
and alloc_else (prof:int) (env:local_env) = function
	|EndI -> End
	|ElseI (_, eL) ->
		let l = List.fold_right (fun (_, e) l -> (alloc_expr prof env e)::l) eL []
		in Else l
	|ElseifI ((_, e), (_, eL), els) ->
		let e = alloc_expr prof env e in
		let l = List.fold_right (fun (_, e) l -> (alloc_expr prof env e)::l) eL [] in
		let els = alloc_else prof env els in
		Elseif (e, l, els)

let alloc_fichier (eL, varMap, sEnv, fMap:fichierTyper):fichier =
	structMap := sEnv;
	let l = List.fold_right (fun e l -> alloc_expr 0 Tmap.empty e::l) eL [] in
	(l, varMap, fMap)

let pushn n =
	let s = ref nop in
	for i = 1 to n do
		s := !s ++ pushq (imm (-1)) ++ pushq !%rax
	done;
	!s

let rec compile_expr = function
	| Entier i -> pushq (imm nTypeInt) ++ pushq (imm64 i)
	| Flottant f -> pushq (imm nTypeFloat) ++ pushq ()
	| Chaine s -> failwith "not implemented"
	| True -> pushq (imm nTypeBool) ++ pushq (imm 1)
	| False -> pushq (imm nTypeBool) ++ pushq (imm 0)
	| Nothing -> pushq (imm nTypeNothing) ++ pushq (imm 0)
	| EntierIdent (entier, label) -> compile_expr (Binop (Times, Entier entier, Ident label))
	| EntierParG (entier, bloc) -> compile_expr (Binop (Times, Entier entier, Bloc bloc))
	| Bloc bloc -> compile_bloc bloc
	| ParDIdent (exp, label) -> compile_expr (Binop (Times, exp, Ident label))
	| Call (ident, funcArbr, expList) ->
		let rec parcours liste =
			match liste with
			| [] -> nop
			| t :: q -> (compile_expr t) ++ (parcours q)
		in
		(parcours expList) ++ (call ident) ++ popn (8 * List.length l) ++ pushq rax
	| Not expr ->
		(compile_expr expr) ++ (popq rbx) ++ (popq rax) ++
		 	(* Teste que c'est bien un booléen *)
		(cmpq nTypeBool rax) ++ (jne 1) (* On aura en 1 la commande pour exit en cas d'erreur!
		 	En 0 il y aura un jmp 2 pour ne pas le trigger au début du programme *)
	| Minus expr -> compile_expr (Binop (Minus, Entier 0, expr))
	| Binop of operateur * expression * expression
	| Ident of label
	| Index of expression * ident * int
	| LvalueAffectV of label * expression
	| LvalueAffectI of expression * ident * int * expression
	| Ret of pjtype * expression (* expected type of the return *)
	| For of int * expression * expression * bloc
	| While of expression * int * bloc
	| If of expression * bloc * else_
and compile_else_ =
	| End
	| Else of bloc
	| Elseif of expression * bloc * else_
and compile_bloc = (expression list)
