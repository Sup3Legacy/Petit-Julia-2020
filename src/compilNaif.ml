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

let rec alloc_expr (env: local_env) (offset:int):Astype.exprTyper -> (AstcompilN.expression * int) = function
	| EntierE i -> Entier i, offset
	| FlottantE f -> Flottant f, offset
	| ChaineE s -> Chaine s, offset
	| TrueE -> True, offset
	| FalseE -> False, offset
	| BlocE (_, eL) ->
		(Bloc (
			List.fold_right
				(fun (_, e) l -> let (e1, _) = alloc_expr env offset e in e1::l)
				eL
				[]), offset)
	| CallE _ -> failwith "Not implemented"
	| NotE (_,e) -> let (e, o) = alloc_expr env offset e in Not e, o
	| MinusE (_, e) -> let (e, o) = alloc_expr env offset e in Minus e, o
	| BinopE (o, (_, e1), (_, e2)) ->
		let (e1, o1) = alloc_expr env offset e1 in
		let (e2, o2) = alloc_expr env offset e2 in
		Binop (o, e1, e2), min o1 o2
	| LvalueE l -> begin
		match l with
			| IdentL (_, ident, true) -> Ident (Dec (Tmap.find ident env)), offset
			| IdentL (_, ident, false) -> Ident (Tag ident), offset
			| IndexL ((_, e), nameS, nameC) ->
				let (e, offset) = alloc_expr env offset e in
				let i2 = assert false in
				Index (e, nameS, i2), offset
		end
	| LvalueAffectE (l, (_, e)) -> begin
		let e, offset = alloc_expr env offset e in
		match l with
			| IdentL (_, ident, false) -> LvalueAffectV ((Tag ident), e), offset
			| IdentL (_, ident, true) -> LvalueAffectV (Dec (Tmap.find ident env), e), offset
			| IndexL ((_, e2), nameS, nameC) ->
				let (e2, o2) = alloc_expr env offset e2 in
				let i2 = assert false in
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
		let env = Tmap.add i offset env2 in
		let (l,o3) = List.fold_right (fun (_, e) (o1, l) -> let e,o2 = (alloc_expr env fpcur2 e) in (e::l, min o1 o2)) eL ([], min fpcur2 (min o1 o2)) in
		For (offset, fpcur2, e1, e2, l), o3
	| WhileE ((_, e), tmap, (_, eL)) ->
		let (e, o1) = alloc_expr env offset e in
		let env2, fpcur2 = Tmap.fold (fun k _ (m, n) -> if k!= i then (Tmap.add k (n-16) m, n-16) else (m, n)) tmap (env, offset) in
		let (l,o2) = List.fold_right (fun (_, e) (o1, l) -> let e,o2 = (alloc_expr env fpcur2 e) in (e::l, min o1 o2)) eL ([], min fpcur2 o1) in
		While (e, fpcur2, l), o2
	| IfE ((_, e), (_, eL), els) ->
		let (e, o1) = alloc_expr env offset e in
		let (l, o2) = List.fold_right (fun (_, e) (o1, l) -> let e,o2 = (alloc_expr env fpcur2 e) in (e::l, min o1 o2)) eL ([], o1) in
		let (els, o3) = alloc_else env offset els in
		If (e, l, els), min o2 o3
and alloc_else (env:local_env) (offset:int) = function
	|EndI -> End, offset
	|ElseI (_, eL) ->
		let (l, o) = List.fold_right (fun (_, e) (o1, l) -> let e,o2 = (alloc_expr env fpcur2 e) in (e::l, min o1 o2)) eL ([], offset)
		in Else l, o
	|ElseifI ((_, e), (_, eL), els) ->
		let (e, o1) = alloc_expr env offset e in
		let (l, o2) = List.fold_right (fun (_, e) (o1, l) -> let e,o2 = (alloc_expr env fpcur2 e) in (e::l, min o1 o2)) eL ([], o1) in
		let (els, o3) = alloc_else env offset els in
		Elseif (e, l, els), min o3 o2

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
	| Flottant f -> pushq (imm nTypeFloat) ++ pushq (immD f) (* À changer *)
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
		compile_expr expr ++ (popq rbx) ++ (popq rax) ++
		(cmpq (imm nTypeBool) !%rax) ++ (jne exitLabel) ++ (* Commande pour exit en cas d'erreur!*)
		(pushq (imm nTypeBool)) ++ (notq !%rax) ++ (pushq !%rax)
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
	  | And -> (cmpq !%rax (imm nTypeBool)) ++ (jne exitLabel) ++ (cmpq !%rcx (imm nTypeBool)) ++ (jne exitLabel) ++
	  							(andq !%rbx !%rdx) ++ (pushq (imm nTypeBool)) ++ (pushq !%rdx)
	  | Or -> (cmpq !%rax (imm nTypeBool)) ++ (jne exitLabel) ++ (cmpq !%rcx (imm nTypeBool)) ++ (jne exitLabel) ++
	  							(orq !%rbx !%rdx) ++ (pushq (imm nTypeBool)) ++ (pushq !%rdx)
		in ins1 ++ ins2 ++ depilation ++ operation
	| Ident label ->
		let offset = 0 in (* /!\ distinction local/global *)
		(pushq (ind ~ofs:offset rbp)) ++ (pushq (ind ~ofs:(offset + 8) rbp))
	| Index (exp, ident, offset) ->
		let numClasse = assert false in
		(compile_expr exp) ++ (popq rbx) ++ (popq rax) ++ (cmpq !%rax (imm numClasse)) ++ (jne exitLabel) ++ (movq (ind ~ofs:(offset + 8) rbx) !%rax) ++ (movq (ind ~ofs:offset rbx) !%rbx)
	| LvalueAffectV (label, expr) -> failwith "Not implemented"
	| LvalueAffectI (exp1, ident, entier, exp2) -> failwith "Not implemented"
	| Ret (pjtype, exp) -> failwith "Not implemented" (* expected type of the return *)
	| For (entier, exp1, exp2, bloc) ->
		let e1 = compile_expr exp1 in
		let e2 = compile_expr exp2 in
		let b = compile_bloc bloc in
		let depile = (popq rdx) ++ (popq rcx) ++ (popq rbx) ++ (popq rax) in
		let test_type = (compq (imm nTypeBool) !%rax) ++ (jne exitLabel) ++ (compq (imm nTypeBool) !%rbx) ++ (jne exitLabel) in
		failwith "Problème : comment est-ce qu'on sauvegarde l'entier ainsi que les bornes? Variables globales?"
	| While (exp, entier, bloc) ->
		let e = compile_expr exp in
		let b = compile_bloc bloc in
		let (label1, label2) = (getWhile (), getWhile ()) in
		let comp = (label label1) ++ e ++ (popq rbx) ++ (popq rax) ++ (cmpq (imm (nTypeBool)) !%rax) ++ (jne exitLabel) ++ (cmpq (imm 1)) ++ (jne label2) in
		let corps = b ++ (jmp label1) ++ (label label2) in
		comp ++ corps
	| If (exp, bloc, else_) ->
		let c = compile_expr exp in
		let c1 = compile_bloc bloc in
		let c2 = compile_else_ else_ in
		let (label1, label2) = (getIf (), getIf ()) in
		c ++ (popq rbx) ++ (popq rax) ++ (cmpq !%rax (imm nTypeBool)) ++ (jne exitLabel) ++
		(cmpq !%rbx (imm 0)) ++ (jne label1) ++ c1 ++ (jmp label2) ++ (label label1) ++ c2 ++ (label label2)
and compile_else_ = function
	| End -> nop
	| Else bloc -> compile_bloc bloc
	| Elseif (exp, bloc, else_) -> compile_expr (If (exp, bloc, else_))
and compile_bloc = function
	| [] -> nop
	| t :: q -> (compile_expr t) ++ (compile_bloc q)


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
