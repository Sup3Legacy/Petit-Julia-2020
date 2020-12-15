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

let valTrue = -1
let valFalse = 0

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp


(* Compteur pour les étiquettes *)
let compteurFor = ref 0
let compteurWhile = ref 0
let compteurIf = ref 0
let compteurFunc = ref 0

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

type local_env = int Smap.t

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
		let (l,o3) = List.fold_right (fun (_, e) (l, o1) -> let e,o2 = (alloc_expr env fpcur2 e) in (e::l, min o1 o2)) eL ([], min fpcur2 (min o1 o2)) in
		For (offset, fpcur2, e1, e2, l), o3
	| WhileE ((_, e), tmap, (_, eL)) ->
		let (e, o1) = alloc_expr env offset e in
		let env2, fpcur2 = Tmap.fold (fun k _ (m, n) -> (Tmap.add k (n-16) m, n-16)) tmap (env, offset) in
		let (l,o2) = List.fold_right (fun (_, e) (l, o1) -> let e,o2 = (alloc_expr env fpcur2 e) in (e::l, min o1 o2)) eL ([], min fpcur2 o1) in
		While (e, offset, fpcur2, l), o2
	| IfE ((_, e), (_, eL), els) ->
		let (e, o1) = alloc_expr env offset e in
		let (l, o2) = List.fold_right (fun (_, e) (l, o1) -> let e,o2 = (alloc_expr env offset e) in (e::l, min o1 o2)) eL ([], o1) in
		let (els, o3) = alloc_else env offset els in
		If (e, l, els), min o2 o3
and alloc_else (env:local_env) (offset:int) = function
	|EndI -> End, offset
	|ElseI (_, eL) ->
		let (l, o) = List.fold_right (fun (_, e) (l, o1) -> let e,o2 = (alloc_expr env offset e) in (e::l, min o1 o2)) eL ([], offset)
		in Else l, o
	|ElseifI ((_, e), (_, eL), els) ->
		let (e, o1) = alloc_expr env offset e in
		let (l, o2) = List.fold_right (fun (_, e) (l, o1) -> let e,o2 = (alloc_expr env offset e) in (e::l, min o1 o2)) eL ([], o1) in
		let (els, o3) = alloc_else env offset els in
		Elseif (e, l, els), min o3 o2

let alloc_fichier (eL, varMap, sEnv, fMap:fichierTyper):fichier =
	structMap := sEnv;
	let (l, o) = List.fold_right (fun e (l, o) -> let (e, o2) = alloc_expr Tmap.empty 0 e in (e::l, min o o2)) eL ([], 0) in
	(l, o, varMap, fMap)

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
	| True -> pushq (imm nTypeBool) ++ pushq (imm valTrue)
	| False -> pushq (imm nTypeBool) ++ pushq (imm valFalse)
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
		let ins2 = compile_expr e2 in
		let (label1, label2) = getIf (), getIf () in
		let depilation = (popq rbx) ++ (popq rax) ++ (popq rdx) ++ (popq rcx) in
		let operation =
		match op with
		| Eq -> (cmpq !%rax !%rcx) ++ (jne exitLabel) ++ (pushq (imm nTypeBool)) ++
							 (cmpq !%rbx !%rcx) ++ (je label1) ++ (pushq (imm valFalse)) ++
							 								   (jmp label2) ++ (label label1) ++ (pushq (imm valTrue)) ++ (label label2)
	  | Neq -> (cmpq !%rax !%rcx) ++ (jne exitLabel) ++ (pushq (imm nTypeBool)) ++
							  (cmpq !%rbx !%rcx) ++ (je label1) ++ (pushq (imm valTrue)) ++
							 								   (jmp label2) ++ (label label1) ++ (pushq (imm valFalse)) ++ (label label2)
	  | Lo -> (cmpq (imm nTypeInt) !%rax) ++ (jne exitLabel) ++
							  (cmpq (imm nTypeInt) !%rcx) ++ (jne exitLabel) ++
							  (pushq (imm nTypeBool)) ++
							  (cmpq !%rbx !%rcx) ++ (jge label1) ++ (pushq (imm valTrue)) ++
							 								   (jmp label2) ++ (label label1) ++ (pushq (imm valFalse)) ++ (label label2)
	  | Gr -> (cmpq !%rax (imm nTypeInt)) ++ (jne exitLabel) ++
							  (cmpq (imm nTypeInt) !%rcx) ++ (jne exitLabel) ++
							  (pushq (imm nTypeBool)) ++
							  (cmpq !%rbx !%rcx) ++ (jle label1) ++ (pushq (imm valTrue)) ++
							 								   (jmp label2) ++ (label label1) ++ (pushq (imm valFalse)) ++ (label label2)
	  | Leq -> (cmpq (imm nTypeInt) !%rax) ++ (jne exitLabel) ++
							   (cmpq (imm nTypeInt) !%rcx) ++ (jne exitLabel) ++
							   (pushq (imm nTypeBool)) ++
							   (cmpq !%rbx !%rcx) ++ (jg label1) ++ (pushq (imm valTrue)) ++
							 								   (jmp label2) ++ (label label1) ++ (pushq (imm valFalse)) ++ (label label2)
	  | Geq -> (cmpq (imm nTypeInt) !%rax) ++ (jne exitLabel) ++
							   (cmpq (imm nTypeInt) !%rcx) ++ (jne exitLabel) ++
							   (pushq (imm nTypeBool)) ++
							   (cmpq !%rbx !%rcx) ++ (jl label1) ++ (pushq (imm valTrue)) ++
							 								   (jmp label2) ++ (label label1) ++ (pushq (imm valFalse)) ++ (label label2)
	  | Plus -> (cmpq (imm nTypeInt) !%rax) ++ (jne exitLabel) ++
							    (cmpq (imm nTypeInt) !%rcx) ++ (jne exitLabel) ++
							    (pushq (imm nTypeInt)) ++
							    (addq !%rcx !%rax) ++ (pushq !%rax)
	  | Minus -> (cmpq (imm nTypeInt) !%rax) ++ (jne exitLabel) ++
							    (cmpq (imm nTypeInt) !%rcx) ++ (jne exitLabel) ++
							    (pushq (imm nTypeInt)) ++
							    (subq !%rcx !%rax) ++ (pushq !%rax)
	  | Times -> (cmpq (imm nTypeInt) !%rax) ++ (jne exitLabel) ++
							    (cmpq (imm nTypeInt) !%rcx) ++ (jne exitLabel) ++
							    (pushq (imm nTypeInt)) ++
							    (imulq !%rdx !%rax) ++ (pushq !%rax)
	  | Modulo -> failwith "Not implemented"
	  | Exp -> failwith "Not implemented"
	  | And -> (cmpq (imm nTypeBool) !%rax) ++ (jne exitLabel) ++ (cmpq (imm nTypeBool) !%rcx) ++ (jne exitLabel) ++
	  							(andq !%rbx !%rdx) ++ (pushq (imm nTypeBool)) ++ (pushq !%rdx)
	  | Or -> (cmpq (imm nTypeBool) !%rax) ++ (jne exitLabel) ++ (cmpq (imm nTypeBool) !%rcx) ++ (jne exitLabel) ++
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
	| For (posC, posFLoc, exp1, exp2, bloc) ->
		let e1 = compile_expr exp1 in
		let e2 = compile_expr exp2 in
		let b = compile_bloc bloc in
		let depile = (popq rdx) ++ (popq rcx) ++ (popq rbx) ++ (popq rax) in
		let test_type = (cmpq (imm nTypeBool) !%rax) ++ (jne exitLabel) ++ (cmpq (imm nTypeBool) !%rbx) ++ (jne exitLabel) in
		failwith "Problème : comment est-ce qu'on sauvegarde l'entier ainsi que les bornes? Variables globales?"
	| While (exp, debLoc, finLoc, bloc) ->
		let e = compile_expr exp in
		let b = compile_bloc bloc in
		let (label1, label2) = (getWhile (), getWhile ()) in
		let comp = (label label1) ++ e ++ (popq rbx) ++ (popq rax) ++ (cmpq (imm (nTypeBool)) !%rax) ++ (jne exitLabel) ++ (cmpq !%rbx (imm valTrue)) ++ (jne label2) in
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


let compile_fun n i = function
  |Funct (argL, tmap, eL) ->
  	let code = nop in
    pushq !%rbp ++ movq !%rsp !%rbp ++
    code ++
    popq rbx ++ popq rax ++
    movq !%rbp !%rsp ++ popq rbp ++
    ret
  |StructBuilder eL ->
    let nType = assert false in
    let n = List.length eL in
    let code = ref nop in
    for i = 0 to -1 do
      code := !code ++
        movq (ind ~ofs:(8+i*16) rsp) (ind ~ofs:(16*(n-i-1)) rax) ++
        movq (ind ~ofs:(16+i*16) rsp) (ind ~ofs:(16*(n-i-1)+8) rax)
    done;
    movq (imm (16*n)) !%rdi ++ call "malloc" ++ !code ++
    movq (!%rax) !%rbx ++ movq (imm nType) !%rax ++
    ret

let compile_program f ofile =
 let (eL, i, smap, fmap) = alloc_fichier f in
 let code = List.fold_left (fun d e -> (if d!=nop then d ++ popn 16 else nop) ++ compile_expr e) nop eL in
 let codefun = Tmap.fold (fun k imap asm -> Imap.fold (fun i f asm2 -> asm2 ++ compile_fun k i f) imap asm) fmap nop in
 let p =
   { text =
		globl "main" ++ label "main" ++
		pushq !%rbx ++ pushq !%r12 ++
		pushq !%rbp ++ movq !%rsp !%r12 ++
		movq !%rsp !%rbp ++
		pushn i ++
		code ++
		movq !%rbp !%rsp ++
		popq rbp ++
		popq r12 ++ popq rbx ++
		movq (imm 0) !%rax ++ (* exit *)
		ret ++

		label "print_value" ++ (*Il doit y avoir le type dans rax et la valur dans rbx*)
		(movq !%rbx !%rdi) ++
		(cmpq (imm nTypeInt) !%rax) ++
		je "ifInt" ++
		(cmpq (imm nTypeFloat) !%rax) ++
		je "ifFloat" ++
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
		movq (ilab ".Sprint_int") !%rdi ++
		movq (imm 0) !%rax ++
		call "printf" ++
    ret ++

		label "print_float" ++
		movq !%rdi !%rsi ++
		movq (ilab ".Sprint_float") !%rdi ++
		movq (imm 0) !%rax ++
		call "printf" ++
    ret ++

		label "print_string" ++
		movq !%rdi !%rsi ++
		movq (ilab ".Sprint_string") !%rdi ++
		movq (imm 0) !%rax ++
		call "printf" ++
    ret ++

		label "print_bool" ++
		movq !%rdi !%rsi ++
		movq (ilab ".Sprint_bool") !%rdi ++
		movq (imm 0) !%rax ++
		call "printf" ++
    ret ++

		label exitLabel ++
		movq !%r12 !%rsp ++
		popq rbp ++
		popq r12 ++ popq rbx ++
		movq (imm 1) !%rax ++
		ret ++
       codefun;
     data =
       (*Hashtbl.fold (fun x _ l -> label x ++ dquad [1] ++ l) genv*)

       (label ".Sprint_int" ++ string "%d") ++
			 (label ".Sprint_float" ++ string "%f") ++
			 (label ".Sprint_string" ++ string "%s") ++
			 (label ".Sprint_bool" ++ string "%q") ++
			 (label ".Sprint_endline" ++ string "\n") ++
			 (label ".Sprint_true" ++ string "true") ++
			 (label ".Sprint_false" ++ string "false")
   }
 in
 let f = open_out ofile in
 let fmt = Format.formatter_of_out_channel f in
 X86_64.print_program fmt p;
 Format.fprintf fmt "@?";
 close_out f


(*
label "print_int" ++
movq !%rdi !%rsi ++
movq (ilab ".Sprint_int") !%rdi ++
movq (imm 0) !%rax ++
call "printf" ++
ret
avec dans data : (label ".Sprint_int" ++ string "%d\n")
*)
