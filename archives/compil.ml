(*open Astype
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

let valTrue = -1
let valFalse = 0

(* Compteur pour les étiquettes *)
let compteurFor = ref 0
let compteurWhile = ref 0
let compteurIf = ref 0
let compteurFunc = ref 0
let compteurArbreAppel = ref 0

let compteurString = ref 0
let compteurFloat = ref 0

let sMap = Hashtbl.create 8
let fMap = Hashtbl.create 8

let getFor () =
	let temp = !compteurFor in
	compteurFor := !compteurFor + 1;
	("for" ^ (string_of_int temp))
;;
let getWhile () =
	let temp = !compteurWhile in
	compteurWhile := !compteurWhile + 1;
	("while" ^ (string_of_int temp))
;;
let getIf () =
	let temp = !compteurIf in
	compteurIf := !compteurIf + 1;
	("if" ^ (string_of_int temp))
;;
let getFunc () =
	let temp = !compteurFunc in
	compteurFunc := !compteurFunc + 1;
	("func" ^ (string_of_int temp))
;;

let newFlagArb () =
	let t = !compteurArbreAppel in
	compteurArbreAppel := 1 +t;
	"jmp"^string_of_int t

let typeLabel = "typeError"
let undefLabel = "undefError"
let zeroDivLabel = "zeroDivError"

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

module Smap = Map.Make(String)

let structMap = ref (Smap.empty: Astype.structEnv)
let functionMap = ref (Smap.empty: Astype.funcMap)

type local_env = int Smap.t

let numStruct ident = nTypeStruct + snd (Smap.find ident !structMap)

let int_of_type t = match t with
  | Any -> nTypeUndef
  | Nothing -> nTypeNothing
  | Int64 -> nTypeInt
  | Float64 -> nTypeFloat
  | Bool -> nTypeBool
  | String -> nTypeString
  | S s -> numStruct s

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

let rec buildArb (p:int) (f:string) (l:string):functArbr -> [`text] asm = function
	| Failure -> (label l ++ jmp exitLabel)
	| Feuille (s,i) -> label l ++ call (s^"_"^string_of_int i) ++ jmp f
	| Appels tM ->
		if TypeMap.cardinal tM == 1 then
			if TypeMap.mem Any tM then buildArb (p-1) f l (TypeMap.find Any tM)
			else
				let (t,arb) = TypeMap.choose tM in
				label l ++ cmpq (imm (int_of_type t)) (ind ~ofs:(16*p - 8) rsp) ++ jne exitLabel ++ buildArb (p-1) f (newFlagArb ()) arb
		else
			let (c1,l1) = TypeMap.fold (fun k a (c,l) -> if k=Any then (c,l)
								else let dir = newFlagArb () in
								(c ++ cmpq (imm (int_of_type k)) (ind ~ofs:(16*p - 8) rsp) ++ je dir, (dir,a)::l)
							) tM (label l, []) in
			let c = if TypeMap.mem Any tM then
					buildArb (p-1) f (newFlagArb ()) (TypeMap.find Any tM)
				else
					jmp exitLabel in
			let c2 = List.fold_left (fun c (dir,a) -> c ++ buildArb (p-1) f dir a) (c1 ++ c) l1 in
			c2

let rec compile_expr (env : local_env) (offset:int) = function
	| EntierE i -> movq (imm64 i) !%rax, offset
	| FlottantE f ->
		let s = string_of_float f in
		let i = if Hashtbl.mem fMap s then
			Hashtbl.find fMap s
		else begin
			let c = !compteurFloat in
			Hashtbl.add fMap s c;
			compteurFloat := c + 1;
			c
			end
		in movq ((if estMac then lab else ilab) ("float"^string_of_float i) %rax, offset
	| ChaineE s ->
		let i = if Hashtbl.mem sMap s then
			Hashtbl.find sMap s
		else begin
			let c = !compteurString in
			Hashtbl.add sMap s c;
			compteurString := c + 1;
			c
			end	
		in movq ((if estMac then lab else ilab) ("string"^string_of_float i) %rax, offset
	| TrueE -> movq (imm valTrue) !%rax
	| FalseE -> movq (imm valFalse) !%rax
	| BlocE b -> if b = [] 
		then movq (imm nTypeNothing) !%rbx, offset
		else
			List.fold_left (fun (c,o) -> let (c2,o2) = compile_expr env offset in (c++c2, min o o2)) (nop, offset)
	| CallE  (funcSet, expressionTyper_list) -> assert false
	| NotE (t,e) -> begin
		let e, offset = compile_expr env offset in (e ++
			match t with
				|Bool -> notq !%rax
				|Any -> cmpq (imm nTypeBool) !%rcx ++ jne typeLabel ++ notq !%rax
				|_ -> assert false
			, offset)
		end
	| MinusE (t,e) -> begin
		let e, offset = compile_expr env offset in (e ++
			match t with
				|Int64 -> negq !%rax
				|Float64 -> mulsd (imm 1) !%rax
				|Any -> cmpq (imm nTypeInt) !%rcx ++ jne typeLabel ++ negq !%rax
				|_ -> assert false
			, offset)
		end
	| BinopE (o,(t1,e1), (t2,e2)) ->
		let e1, o1 = compile_expr env offset e1 in
		let e2, o2 = compile_expr env offset e2 in
		let offset = min o1 o2 in (
			match o,t1,t2 with
				|Eq, Any, Any -> 
					let lab1, lab2 = getIf (), getIf () in
					e1 ++ pushq !%rax ++ pushq !%rcx ++ e2 ++ popq r9 ++ popq r8 ++
					cmpq !%rax !%r8 ++ jne lab1 ++ cmpq !%rcx !%r9 ++ jne lab1 ++
					movq (imm valTrue) !%rax ++ jmp lab2 ++
					label lab1 ++ xorq !%rax !%rax ++ label lab2
				|Eq, _, _ when t1 = t2->
					let label1 = getIf () in
					e1 ++ pushq !%rax ++ pushq !%rax ++ e2 ++ popq rcx ++ popq rcx ++
					movq !%rax !%r8 ++ xorq !%rax !%rax ++
					cmpq !%r8 !%rcx ++ jne label1 ++ movq (imm valTrue) !%rax ++ label label1
				|Eq, Any,_ ->
					let lab1, lab2 = getIf (), getIf () in
					e1 ++ pushq !%rax ++ pushq !%rcx ++ e2 ++ popq r9 ++ popq r8 ++
					cmpq !%rax !%r8 ++ jne lab1 ++ cmpq (imm (int_of_type t2) !%r9) ++
					jne lab1 ++ movq (imm valTrue) !%rax ++ jmp lab2 ++
					label lab1 ++ xorq !%rax !%rax ++ label lab2
				|Eq, _, Any ->
					let lab1, lab2 = getIf (), getIf () in
					e1 ++ pushq !%rax ++ pushq !%rcx ++ e2 ++ popq r9 ++ popq r8 ++
					cmpq !%rax !%r8 ++ jne lab1 ++ cmpq (imm (int_of_type t1) !%rcx) ++
					jne lab1 ++ movq (imm valTrue) !%rax ++ jmp lab2 ++
					label lab1 ++ xorq !%rax !%rax ++ label lab2
				|Eq, _, _ -> e1 ++ e2 ++ xorq !%rax !%rax
				|Neq, Any, Any -> 
					let lab1, lab2 = getIf (), getIf () in
					e1 ++ pushq !%rax ++ pushq !%rcx ++ e2 ++ popq r9 ++ popq r8 ++
					cmpq !%rax !%r8 ++ jne lab1 ++ cmpq !%rcx !%r9 ++ jne lab1 ++
					xorq !%rax !%rax ++ jmp lab2 ++
					label lab1 ++ movq (imm valTrue) !%rax ++ label lab2
				|Neq, _, _ when t1 = t2->
					let lab1, lab2 = getIf (), getIf () in
					e1 ++ pushq !%rax ++ pushq !%rax ++ e2 ++ popq rcx ++ popq rcx ++
					cmpq !%rax !%rcx ++ je lab1 ++ movq (imm valTrue) !%rax ++ 
					jmp lab2 ++ label lab1 ++ xorq !%rax !%rax ++ label lab2
				|Neq, Any,_ ->
					let lab1, lab2 = getIf (), getIf () in
					e1 ++ pushq !%rax ++ pushq !%rcx ++ e2 ++ popq r9 ++ popq r8 ++
					cmpq !%rax !%r8 ++ jne lab1 ++ cmpq (imm (int_of_type t2) !%r9) ++
					jne lab1 ++ xorq !%rax !%rax ++ jmp lab2 ++
					label lab1 ++ movq (imm valTrue) !%rax ++ label lab2
				|Neq, _, Any ->
					let lab1, lab2 = getIf (), getIf () in
					e1 ++ pushq !%rax ++ pushq !%rcx ++ e2 ++ popq r9 ++ popq r8 ++
					cmpq !%rax !%r8 ++ jne lab1 ++ cmpq (imm (int_of_type t1) !%rcx) ++
					jne lab1 ++ xorq !%rax !%rax ++ jmp lab2 ++
					label lab1 ++ movq (imm valTrue) !%rax ++ label lab2
				|Neq, _, _ -> e1 ++ e2 ++ movq (imm valTrue) !%rax
			, offset)
	| LvalueE of lvalueTyper
	| LvalueAffectE of lvalueTyper * expressionTyper
	| ReturnE of pjtype * (expressionTyper option) (* expected type of the return *)
	| ForE of ident * pjtype Tmap.t * expressionTyper * expressionTyper * blocTyper
	| WhileE of expressionTyper * pjtype Tmap.t * blocTyper
	| IfE of expressionTyper * blocTyper * elseTyper
and lvalueTyper =
	| IdentL of pjtype * ident * bool
	| IndexL of expressionTyper * ident * ident (* expression, name of struct, name of value *)
and elseTyper =
	| EndI
	| ElseI of blocTyper
	| ElseifI of expressionTyper * blocTyper * elseTyper*)
