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

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp


(* Compteur pour les étiquettes *)
let compteurFor = ref 0
let compteurWhile = ref 0
let compteurIf = ref 0

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


let exitLabel = "exit"

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
	| BlocE (_, eL) ->
		Bloc (
			List.fold_right
				(fun (_, e) l -> (alloc_expr prof env e)::l)
				eL
				[])
(*	| EntierIdentE (i, _,  nm, true) -> EntierIdent (i, Dec (calcPos prof nm env))
	| EntierIdentE (i, _,  nm, false) -> EntierIdent (i, Tag nm)
	| EntierParGE  (i, (_, eL)) ->
		let l = List.fold_right
			(fun (_, e) l -> (alloc_expr prof env e)::l)
			eL
			[]
		in EntierParG (i, l)

	| ParDIdentE ((_, e), _, nm, true) ->
		ParDIdent (alloc_expr prof env e, Dec (calcPos prof nm env))
	| ParDIdentE ((_, e), _, nm, false) ->
		ParDIdent (alloc_expr prof env e, Tag nm)*)
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
	| Flottant f -> pushq (imm nTypeFloat) ++ pushq (imm64 (Int64.of_int 0)) (* À changer *)
	| Chaine s -> failwith "not implemented"
	| True -> pushq (imm nTypeBool) ++ pushq (imm 1)
	| False -> pushq (imm nTypeBool) ++ pushq (imm 0)
	| Nothing -> pushq (imm nTypeNothing) ++ pushq (imm 0)
	| Bloc bloc -> compile_bloc bloc
(*	| EntierIdent (entier, label) -> compile_expr (Binop (Times, Entier entier, Ident label))
	| EntierParG (entier, bloc) -> compile_expr (Binop (Times, Entier entier, Bloc bloc))
	| ParDIdent (exp, label) -> compile_expr (Binop (Times, exp, Ident label))*)
	| Call (ident, funcArbr, expList) ->
		let rec parcours liste =
			match liste with
			| [] -> nop
			| t :: q ->
				let eq = parcours q in
				let e = compile_expr t in
			e ++ eq
		in
		let e = parcours expList in
		e ++ (call ident) ++ popn (8 * List.length expList) ++ (pushq !%rax)
	| Not expr ->
		let (label1, label2) = (getIf (), getIf ()) in
		compile_expr expr ++ (popq rbx) ++ (popq rax) ++
		 	(* Teste que c'est bien un booléen *)
		(cmpq (imm nTypeBool) !%rax) ++ (jne exitLabel) ++ (* Commande pour exit en cas d'erreur!*)
		(pushq (imm nTypeBool)) ++ (cmpq (imm 0) !%rax) ++ (je label1) ++ (pushq (imm 0)) ++
		(jmp label2) ++ (label label1) ++ (pushq (imm 1)) ++ (label label2)
	| Minus expr -> compile_expr (Binop (Minus, Entier (Int64.of_int 0), expr))
	| Binop (op, e1, e2) ->
		let ins1 = compile_expr e1 in
		let ins2 = compile_expr  e2 in
		let (label1, label2) = getIf (), getIf () in
		let depilation = (popq rbx) ++ (popq rax) ++ (popq rdx) ++ (popq rcx) in
		let operation =
		match op with
		| Eq -> (cmpq !%rax !%rcx) ++ (jne exitLabel) ++ (pushq (imm nTypeBool)) ++
							 (cmpq !%rbx !%rcx) ++ (je label1) ++ (pushq (imm 0)) ++
							 								   (jmp label2) ++ (label label1) ++ (pushq (imm 1)) ++ (label label2)
	  | Neq -> (cmpq !%rax !%rcx) ++ (jne exitLabel) ++ (pushq (imm nTypeBool)) ++
							  (cmpq !%rbx !%rcx) ++ (je label1) ++ (pushq (imm 1)) ++
							 								   (jmp label2) ++ (label label1) ++ (pushq (imm 0)) ++ (label label2)
	  | Lo -> (cmpq !%rax (imm nTypeInt)) ++ (jne exitLabel) ++
							  (cmpq !%rcx (imm nTypeInt)) ++ (jne exitLabel) ++
							  (pushq (imm nTypeBool)) ++
							  (cmpq !%rbx !%rcx) ++ (jge label1) ++ (pushq (imm 1)) ++
							 								   (jmp label2) ++ (label label1) ++ (pushq (imm 0)) ++ (label label2)
	  | Gr -> (cmpq !%rax (imm nTypeInt)) ++ (jne exitLabel) ++
							  (cmpq !%rcx (imm nTypeInt)) ++ (jne exitLabel) ++
							  (pushq (imm nTypeBool)) ++
							  (cmpq !%rbx !%rcx) ++ (jle label1) ++ (pushq (imm 1)) ++
							 								   (jmp label2) ++ (label label1) ++ (pushq (imm 0)) ++ (label label2)
	  | Leq -> (cmpq !%rax (imm nTypeInt)) ++ (jne exitLabel) ++
							   (cmpq !%rcx (imm nTypeInt)) ++ (jne exitLabel) ++
							   (pushq (imm nTypeBool)) ++
							   (cmpq !%rbx !%rcx) ++ (jg label1) ++ (pushq (imm 1)) ++
							 								   (jmp label2) ++ (label label1) ++ (pushq (imm 0)) ++ (label label2)
	  | Geq -> (cmpq !%rax (imm nTypeInt)) ++ (jne exitLabel) ++
							   (cmpq !%rcx (imm nTypeInt)) ++ (jne exitLabel) ++
							   (pushq (imm nTypeBool)) ++
							   (cmpq !%rbx !%rcx) ++ (jl label1) ++ (pushq (imm 1)) ++
							 								   (jmp label2) ++ (label label1) ++ (pushq (imm 0)) ++ (label label2)
	  | Plus -> (cmpq !%rax (imm nTypeInt)) ++ (jne exitLabel) ++
							    (cmpq !%rcx (imm nTypeInt)) ++ (jne exitLabel) ++
							    (pushq (imm nTypeInt)) ++
							    (addq !%rcx !%rax) ++ (pushq !%rax)
	  | Minus -> (cmpq !%rax (imm nTypeInt)) ++ (jne exitLabel) ++
							    (cmpq !%rcx (imm nTypeInt)) ++ (jne exitLabel) ++
							    (pushq (imm nTypeInt)) ++
							    (subq !%rcx !%rax) ++ (pushq !%rax)
	  | Times -> (cmpq !%rax (imm nTypeInt)) ++ (jne exitLabel) ++
							    (cmpq !%rcx (imm nTypeInt)) ++ (jne exitLabel) ++
							    (pushq (imm nTypeInt)) ++
							    (imulq !%rdx !%rax) ++ (pushq !%rax)
	  | Modulo -> failwith "Not implemented"
	  | Exp -> failwith "Not implemented"
	  | And -> failwith "Not implemented"
	  | Or -> failwith "Not implemented"
	in ins1 ++ ins2 ++ depilation ++ operation
	| Ident label ->
		let offset = 0 in (* /!\ distinction local/global *)
		(pushq (ind ~ofs:offset rbp)) ++ (pushq (ind ~ofs:(offset + 8) rbp))
	| Index (exp, ident, entier) ->
		failwith "Not implemented"
	| LvalueAffectV (label, expr) -> failwith "Not implemented"
	| LvalueAffectI (exp1, ident, entier, exp2) -> failwith "Not implemented"
	| Ret (pjtype, exp) -> failwith "Not implemented" (* expected type of the return *)
	| For (entier, exp1, exp2, bloc) -> failwith "Not implemented"
	| While (exp, entier, bloc) ->
		let e = compile_expr exp in
		let code = (popq rax) ++ (cmpq (imm (nTypeBool)) !%rax) ++ (jne exitLabel) in
		failwith "Not implemented"
	| If (exp, bloc, else_) -> failwith "Not implemented"
and compile_else_ = function
	| End -> failwith "Not implemented"
	| Else bloc -> failwith "Not implemented"
	| Elseif (exp, bloc, else_) -> failwith "Not implemented"
and compile_bloc = failwith "Not implemented"


let compile_function f e fpmax =
	let code =
		label f ++
		pushq !%rbp ++
		movq !%rsp !%rbp ++ pushn fpmax ++
		compile_expr e ++ popq rax ++
		popn fpmax ++ popq rbp ++ ret
	in
	code

(*
let compile_program p ofile =
 let p = alloc p in
 Format.eprintf "%a@." print p;
 let codefun, code = List.fold_left compile_stmt (nop, nop) p in
 let p =
   { text =
       globl "main" ++ label "main" ++
       movq !%rsp !%rbp ++
       code ++
       movq (imm 0) !%rax ++ (* exit *)
       ret ++
       label "print_int" ++
       movq !%rdi !%rsi ++
       movq (ilab ".Sprint_int") !%rdi ++
       movq (imm 0) !%rax ++
       call "printf" ++
       ret ++
       codefun;
     data =
       Hashtbl.fold (fun x _ l -> label x ++ dquad [1] ++ l) genv
         (label ".Sprint_int" ++ string "%d\n")
   }
 in
 let f = open_out ofile in
 let fmt = formatter_of_out_channel f in
 X86_64.print_program fmt p;
 fprintf fmt "@?";
 close_out f
*)
