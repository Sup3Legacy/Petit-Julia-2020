open Astype
open AstcompilN
open X86_64

let nTypeUndef = -1
let nTypeNothing = 0
let nTypeInt = 1
let nTypeFloat = 2
let nTypeBool = 3
let nTypeString = 4
let nTypeChar = 5
let nTypeStruct = 6
(* À partir de 6 : structs *)
let nTypeArray = ref 65536 (* un array d'int à n dimension aura comme type : nTypeInt + n * nTypeArray *)


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

let compteurNewArray = ref 0

(* These are for analytics *)
let compteurCall = ref 10
let compteurMalloc = ref 0

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
let getNewArray () =
	let temp = !compteurNewArray in
	compteurNewArray := !compteurNewArray + 1;
	temp
;; 

let rectify_character str =
	String.concat "." (List.filter (fun x -> x <> "") (String.split_on_char ':' str))
;;


(*let exitLabel = "exit"*)
let errorUndef = "undef"
let error0div = "divBy0"
let errorTypeE = "typingError"
let errorCall1 = "call1Error"
let errorCall2 = "call2Error"
let errorOoB = "outOfBound"
let errorAssert = "assertError"

module Smap = Map.Make(String)

let structMap = ref (Smap.empty: Astype.structEnv)
let functionMap = ref (Smap.empty: Astype.funcMap)
let functionEnv = ref (Smap.empty: Astype.funcEnv)

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
	| CharE c -> Char c, offset
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
		let () = match ident with
			| "div" -> CompilRef.div := true
			| "float" -> CompilRef.div := true
			| "char" -> CompilRef.div := true
			| "int" -> CompilRef.div := true
			| "input_int" -> CompilRef.input_int := true
			| _ -> ()
		in
		compteurCall := !compteurCall + 1;
		let (offset, eL) =
			List.fold_right
				(fun (_, e) (o1, l) -> let (e1, o2) = alloc_expr env offset e in (min o1 o2, e1::l))
				eL
				(offset, [])
		in
		match ident with 
		| "print" | "_getelement" | "_setelement" | "newarray" -> Call (ident, Feuille (ident, 0), eL), offset
		| _ ->
			let f1 = try Tmap.find ident !functionEnv with Not_found -> failwith ("not found function "^ident) in
			let f = List.fold_left (fun m (i,l,s) -> Imap.add i l m) Imap.empty f1 in
			let arb:AstcompilN.functArbr = calcArb ident (List.fold_left (fun l (i,p,s) -> if ISet.mem i iSet then (p, i, 0)::l else l) [] f1)
			in Call (ident, arb, eL), offset
		end
	| NotE (_,e) -> let (e, o) = alloc_expr env offset e in Not e, o
	| AssertE (l, n, (_,e)) ->
		let () = if not (Hashtbl.mem sMap n)
		then
			(Hashtbl.add sMap n !compteurString;
			compteurString := !compteurString + 1)
		in let (e, o) = alloc_expr env offset e in (Assert (l,n,e), o)
	| MinusE (_, e) -> let () = CompilRef.sub := true in let (e, o) = alloc_expr env offset e in Minus e, o
	| BinopE (o, (_, e1), (_, e2)) ->
		let () = match o with 
			|Plus -> CompilRef.add := true
			|Times -> CompilRef.mul := true
			|Minus -> CompilRef.sub := true
			|Geq -> CompilRef.ge := true
			|Gr -> CompilRef.g := true
			|Leq -> CompilRef.le := true
			|Lo -> CompilRef.l := true
			| _  -> ()
		in 
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
			| ArrayL ((_, e1), (_, e2)) -> 
				let (e1, o1) = alloc_expr env offset e1 in
				let (e2, o2) = alloc_expr env offset e2 in
				Array (e1, e2), min o1 o2
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
			| ArrayL ((_, e1), (_, e2)) -> 
				let (e1, o1) = alloc_expr env offset e1 in
				let (e2, o2) = alloc_expr env offset e2 in
				LvalueAffectA (e1, e2, e), min offset (min o1 o2)
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
	| DoWhileE (tmap, (_, eL)) ->
		let env2, fpcur2 = Tmap.fold (fun k _ (m, n) -> (Tmap.add k (n-16) m, n-16)) tmap (env, offset) in
		let (l,o2) = List.fold_right (fun (_, e) (l, o1) -> let e,o2 = (alloc_expr env2 fpcur2 e) in (e::l, min o1 o2)) eL ([], fpcur2) in
		DoWhile (offset, fpcur2, l), o2		
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

let alloc_fichier (eL, varMap, sEnv, fMap, fEnv:fichierTyper):fichier =
	structMap := sEnv;
	functionMap := fMap;
	functionEnv := fEnv;
	nTypeArray := 100 + Smap.cardinal sEnv;
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
	| String -> nTypeChar + !nTypeArray
	| Array -> !nTypeArray
	| Char64 -> nTypeChar
 	| S s -> numStruct s

let newFlagArb () =
	let t = !compteurArbreAppel in
	compteurArbreAppel := 1 +t;
	"jmp"^string_of_int t

let rec buildArb (p:int) (f:string) (l:string):functArbr -> [`text] asm = function
	| Failure -> (label (rectify_character l) ++ jmp errorCall2)
	| Feuille (s,i) -> compteurCall := !compteurCall + 1;
		label (rectify_character l) ++ call ((rectify_character s)^"_"^string_of_int i) ++ jmp f
	| Appels tM ->
		if TypeMap.cardinal tM == 1 then
			if TypeMap.mem Any tM then buildArb (p-1) f l (TypeMap.find Any tM)
			else
				let (t,arb) = TypeMap.choose tM in
				if (int_of_type t = !nTypeArray) then 
					label (rectify_character l) ++ cmpq (imm (int_of_type t)) (ind ~ofs:(16*p - 8) rsp) ++ jl errorCall1 ++ buildArb (p-1) f (newFlagArb ()) arb
				else label (rectify_character l) ++ cmpq (imm (int_of_type t)) (ind ~ofs:(16*p - 8) rsp) ++ jne errorCall1 ++ buildArb (p-1) f (newFlagArb ()) arb
		else
			let (c1,l1) = TypeMap.fold (fun k a (c,l) -> if k=Any || k = Array then (c,l)
								else let dir = newFlagArb () in
									(c ++ cmpq (imm (int_of_type k)) (ind ~ofs:(16*p - 8) rsp) ++ je dir, (dir,a)::l)
							) tM (label (rectify_character l), []) in
			let c2,l2 = if TypeMap.mem Array tM then 
					let dir = newFlagArb () in
					(c1 ++ cmpq (imm !nTypeArray) (ind ~ofs:(16*p - 8) rsp) ++ jge dir, ((dir, TypeMap.find Array tM)::l1))
				else
					c1,l1 in
			let c = if TypeMap.mem Any tM then
					buildArb (p-1) f (newFlagArb ()) (TypeMap.find Any tM)
				else
					jmp errorCall1 in
			let c3 = List.fold_left (fun c (dir,a) -> c ++ buildArb (p-1) f dir a) (c2 ++ c) l2 in
			c3


let rec compile_expr = function
	| Entier i -> pushq (imm nTypeInt) ++ movq (imm64 i) !%rax ++ pushq !%rax
	| Flottant f -> (*pushq (imm nTypeFloat) ++ pushq (immD f)*) (* À changer *)
		let n = string_of_int (Hashtbl.find fMap (string_of_float f)) in
		pushq (imm nTypeFloat) ++ pushq ((if estMac then lab else ilab) ("constant_float"^n))
	| Char c -> pushq (imm nTypeChar) ++ pushq (imm c)
	| Chaine s ->
		let n = String.length s in
		let n2 = (n + 2) * 8 in
		let depl = ref (movq (imm (nTypeChar + !nTypeArray)) (ind ~ofs:0 rax) ++ movq (imm n) (ind ~ofs:8 rax) ) in
		for i = 0 to n-1 do
			depl := !depl ++ (movq (imm (Char.code s.[i])) (ind ~ofs:(i*8+16) rax))
		done;
		movq (imm n2) !%rdi ++ call "malloc" ++ !depl ++ pushq (imm (nTypeChar + !nTypeArray)) ++ pushq !%rax
	| True -> pushq (imm nTypeBool) ++ pushq (imm valTrue)
	| False -> pushq (imm nTypeBool) ++ pushq (imm valFalse)
	| Nothing -> pushq (imm nTypeNothing) ++ pushq (imm 0)
	| Bloc bloc -> compile_bloc bloc
	| Assert (l, s, e) -> 
		let ind = Hashtbl.find sMap s in
		compile_expr e ++
		popq rbx ++ popq rax ++ cmpq (imm nTypeBool) !%rax ++ jne errorTypeE ++
		movq (imm l) !%rdi ++ (if estMac then leaq (lab ("string"^string_of_int ind)) rsi else movq (lab ("string"^string_of_int ind)) !%rsi) ++
		cmpq (imm valTrue) !%rbx ++ jne errorAssert ++ pushq (imm nTypeNothing) ++ pushq !%rax
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
		(match ident with
		| "print" ->
			e ++ (movq (imm (List.length expList)) !%rsi) ++ call "print_0" ++
			popn (16 * List.length expList) ++ (pushq (imm nTypeNothing)) ++ pushq !%rbx
(*		| "input_string" -> 
			let deplq = if estMac then (fun x -> leaq x rdi) else (fun x -> movq x !%rdi) in
			let deplqrsi = if estMac then (fun x -> leaq x rsi) else (fun x -> movq x !%rsi) in
			e ++ 
			deplq (lab ".Sprint_string") ++
			deplqrsi (lab ".Sscan_string") ++
			movq (imm 0) !%rax ++
			call "scanf" ++ 
			movq ((if estMac then lab else ilab) (".Sscan_string")) !%rbx ++ 
			movq (imm nTypeString) !%rax ++
			pushq !%rax ++ pushq !%rbx *)
		| "newarray" -> assert ((List.length expList) = 2); let i1, i2 = getNewArray (), getNewArray () in
			e ++ 
			popq rdx ++ popq rcx ++ (* valeur d'initialisation *)
			popq rbx ++ popq rax ++ (* longueur de l'array *)
			movq !%rbx !%r9 ++ (* On sauvearde la taille*)
			cmpq (imm nTypeInt) !%rax ++ jne errorTypeE ++ (* on vérifie que l'indice est bien entier *)
			addq (imm 2) !%rbx ++ imulq (imm 8) !%rbx ++ (* On ajouter à la taille les 2 mots : type/taille *)
			movq !%rbx !%rdi ++ movq (imm 0) !%rax ++ 
			pushq !%rcx ++ pushq !%rbx ++ pushq !%r9 ++ pushq !%rdx ++ 
			call "malloc" ++ (* /!\ pas d'initialisation pour l'instant *)
			popq rdx ++ popq r9 ++ popq rbx ++ popq rcx ++
			(* L'adresse du début de l'array est en %rax *)
			addq (imm !nTypeArray) !%rcx ++ (* On calcule le nouveau type (ajout de nTypeArray) *)
			movq !%rcx (ind ~ofs:0 rax) ++ (* On stocke le type *)
			movq !%r9 (ind ~ofs:8 rax) ++ (* On stocke la taille *)

			(* Initialisation des champs*)
			label ("init_" ^ (string_of_int i1)) ++ 
			cmpq (imm 0) !%r9 ++ je ("init_" ^ (string_of_int i2)) ++ decq !%r9 ++ 
			movq !%rdx (ind ~ofs:16 ~scale:8 ~index:r9 rax) ++
			jmp ("init_" ^ (string_of_int i1)) ++
			label ("init_" ^ (string_of_int i2)) ++

			movq !%rax !%rbx ++ movq !%rcx !%rax ++ (* Et on met ça sur %rax-%rbx *)
			pushq !%rax ++ pushq !%rbx
		| _ ->
			begin
				let flagfin = newFlagArb () in
				let flagdeb = newFlagArb () in
				e ++ (buildArb (List.length expList) flagfin flagdeb funcArbr) ++
				label flagfin ++
				popn (16 * List.length expList) ++ (pushq !%rax) ++ pushq !%rbx
			end)
	| Not expr ->
		compile_expr expr ++ (popq rbx) ++ (popq rax) ++
		(cmpq (imm nTypeBool) !%rax) ++ (jne errorTypeE) ++ (* Commande pour exit en cas d'erreur!*)
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
		| Eq -> let (label3, label4) = getIf (), getIf () in 
					deb ++
					cmpq (imm nTypeBool) !%rax ++ jne label3 ++ movq (imm nTypeInt) !%rax ++ negq !%rbx ++ (label label3) ++
					cmpq (imm nTypeBool) !%rcx ++ jne label4 ++ movq (imm nTypeInt) !%rcx ++ negq !%rdx ++ (label label4) ++
					(pushq (imm nTypeBool)) ++ (cmpq !%rax !%rcx) ++ (jne label1) ++
							 (cmpq !%rbx !%rdx) ++ (jne label1) ++ (pushq (imm valTrue)) ++
							 								   (jmp label2) ++ (label label1) ++ (pushq (imm valFalse)) ++ (label label2)
		| Neq -> let (label3, label4) = getIf (), getIf () in 
					deb ++ 
				  	cmpq (imm nTypeBool) !%rax ++ jne label3 ++ movq (imm nTypeInt) !%rax ++ negq !%rbx ++ (label label3) ++
					cmpq (imm nTypeBool) !%rcx ++ jne label4 ++ movq (imm nTypeInt) !%rcx ++ negq !%rdx ++ (label label4) ++ 
	  				(pushq (imm nTypeBool)) ++ (cmpq !%rax !%rcx) ++ (jne label1) ++
							  (cmpq !%rbx !%rdx) ++ (jne label1) ++ (pushq (imm valFalse)) ++
							 								   (jmp label2) ++ (label label1) ++ (pushq (imm valTrue)) ++ (label label2)
	  | Lo -> let (label5, label4, label3, labelEnd) = getIf (), getIf (), getIf (), getIf () in
				ins1 ++ ins2 ++
				cmpq (imm nTypeBool) (ind ~ofs:24 rsp) ++ jne label4 ++ movq (imm nTypeInt) (ind ~ofs:24 rsp) ++ negq (ind ~ofs:16 rsp) ++ (label label4) ++
				cmpq (imm nTypeBool) (ind ~ofs:8 rsp) ++ jne label5 ++ movq (imm nTypeInt) (ind ~ofs:8 rsp) ++ negq (ind ~ofs:0 rsp) ++ (label label5) ++
				(cmpq (imm nTypeFloat) (ind ~ofs:24 rsp)) ++ je label1 ++
	  				(cmpq (imm nTypeInt) (ind ~ofs:24 rsp)) ++ jne errorTypeE ++
						(cmpq (imm nTypeInt) (ind ~ofs:8 rsp)) ++ jne label2 ++ call "loII" ++ jmp labelEnd ++
						(label label2) ++ (cmpq (imm nTypeFloat) (ind ~ofs:8 rsp)) ++ jne errorTypeE ++ call "loIF" ++ jmp labelEnd ++
 	  				(label label1) ++
						(cmpq (imm nTypeInt) (ind ~ofs:8 rsp)) ++ jne label3 ++ call "loFI" ++ jmp labelEnd ++
						(label label3) ++ (cmpq (imm nTypeFloat) (ind ~ofs:8 rsp)) ++ jne errorTypeE ++ call "loFF" ++ jmp labelEnd ++
 	  				(label labelEnd) ++ addq (imm 32) !%rsp ++ pushq !%rax ++ pushq !%rbx
	  | Gr -> let (label5, label4, label3, labelEnd) = getIf (), getIf (), getIf (), getIf () in
				ins1 ++ ins2 ++
				cmpq (imm nTypeBool) (ind ~ofs:24 rsp) ++ jne label4 ++ movq (imm nTypeInt) (ind ~ofs:24 rsp) ++ negq (ind ~ofs:16 rsp) ++ (label label4) ++
				cmpq (imm nTypeBool) (ind ~ofs:8 rsp) ++ jne label5 ++ movq (imm nTypeInt) (ind ~ofs:8 rsp) ++ negq (ind ~ofs:0 rsp) ++ (label label5) ++
				(cmpq (imm nTypeFloat) (ind ~ofs:24 rsp)) ++ je label1 ++
					(cmpq (imm nTypeInt) (ind ~ofs:24 rsp)) ++ jne errorTypeE ++
					(cmpq (imm nTypeInt) (ind ~ofs:8 rsp)) ++ jne label2 ++ call "goII" ++ jmp labelEnd ++
					(label label2) ++ (cmpq (imm nTypeFloat) (ind ~ofs:8 rsp)) ++ jne errorTypeE ++ call "goIF" ++ jmp labelEnd ++
				 (label label1) ++
					(cmpq (imm nTypeInt) (ind ~ofs:8 rsp)) ++ jne label3 ++ call "goFI" ++ jmp labelEnd ++
					(label label3) ++ (cmpq (imm nTypeFloat) (ind ~ofs:8 rsp)) ++ jne errorTypeE ++ call "goFF" ++ jmp labelEnd ++
				(label labelEnd) ++ addq (imm 32) !%rsp ++ pushq !%rax ++ pushq !%rbx
	  | Leq -> let (label5, label4, label3, labelEnd) = getIf (), getIf (), getIf (), getIf () in
				ins1 ++ ins2 ++
				cmpq (imm nTypeBool) (ind ~ofs:24 rsp) ++ jne label4 ++ movq (imm nTypeInt) (ind ~ofs:24 rsp) ++ negq (ind ~ofs:16 rsp) ++ (label label4) ++
				cmpq (imm nTypeBool) (ind ~ofs:8 rsp) ++ jne label5 ++ movq (imm nTypeInt) (ind ~ofs:8 rsp) ++ negq (ind ~ofs:0 rsp) ++ (label label5) ++
				(cmpq (imm nTypeFloat) (ind ~ofs:24 rsp)) ++ je label1 ++
					(cmpq (imm nTypeInt) (ind ~ofs:24 rsp)) ++ jne errorTypeE ++
					(cmpq (imm nTypeInt) (ind ~ofs:8 rsp)) ++ jne label2 ++ call "leqII" ++ jmp labelEnd ++
					(label label2) ++ (cmpq (imm nTypeFloat) (ind ~ofs:8 rsp)) ++ jne errorTypeE ++ call "leqIF" ++ jmp labelEnd ++
				(label label1) ++
					(cmpq (imm nTypeInt) (ind ~ofs:8 rsp)) ++ jne label3 ++ call "leqFI" ++ jmp labelEnd ++
					(label label3) ++ (cmpq (imm nTypeFloat) (ind ~ofs:8 rsp)) ++ jne errorTypeE ++ call "leqFF" ++ jmp labelEnd ++
				(label labelEnd) ++ addq (imm 32) !%rsp ++ pushq !%rax ++ pushq !%rbx
	  | Geq -> let (label5, label4, label3, labelEnd) = getIf (), getIf (), getIf (), getIf () in
				ins1 ++ ins2 ++
				cmpq (imm nTypeBool) (ind ~ofs:24 rsp) ++ jne label4 ++ movq (imm nTypeInt) (ind ~ofs:24 rsp) ++ negq (ind ~ofs:16 rsp) ++ (label label4) ++
				cmpq (imm nTypeBool) (ind ~ofs:8 rsp) ++ jne label5 ++ movq (imm nTypeInt) (ind ~ofs:8 rsp) ++ negq (ind ~ofs:0 rsp) ++ (label label5) ++
				(cmpq (imm nTypeFloat) (ind ~ofs:24 rsp)) ++ je label1 ++
					(cmpq (imm nTypeInt) (ind ~ofs:24 rsp)) ++ jne errorTypeE ++
					(cmpq (imm nTypeInt) (ind ~ofs:8 rsp)) ++ jne label2 ++ call "geqII" ++ jmp labelEnd ++
					(label label2) ++ (cmpq (imm nTypeFloat) (ind ~ofs:8 rsp)) ++ jne errorTypeE ++ call "geqIF" ++ jmp labelEnd ++
				(label label1) ++
					(cmpq (imm nTypeInt) (ind ~ofs:8 rsp)) ++ jne label3 ++ call "geqFI" ++ jmp labelEnd ++
					(label label3) ++ (cmpq (imm nTypeFloat) (ind ~ofs:8 rsp)) ++ jne errorTypeE ++ call "geqFF" ++ jmp labelEnd ++
				(label labelEnd) ++ addq (imm 32) !%rsp ++ pushq !%rax ++ pushq !%rbx
	  | Plus -> let (label3, labelEnd) = getIf (), getIf () in
			ins1 ++ ins2 ++ (cmpq (imm nTypeFloat) (ind ~ofs:24 rsp)) ++ je label1 ++
	  		(cmpq (imm nTypeInt) (ind ~ofs:24 rsp)) ++ jne errorTypeE ++
				(cmpq (imm nTypeInt) (ind ~ofs:8 rsp)) ++ jne label2 ++ call "addII" ++ jmp labelEnd ++
				(label label2) ++ (cmpq (imm nTypeFloat) (ind ~ofs:8 rsp)) ++ jne errorTypeE ++ call "addIF" ++ jmp labelEnd ++
 	  		(label label1) ++
				(cmpq (imm nTypeInt) (ind ~ofs:8 rsp)) ++ jne label3 ++ call "addFI" ++ jmp labelEnd ++
				(label label3) ++ (cmpq (imm nTypeFloat) (ind ~ofs:8 rsp)) ++ jne errorTypeE ++ call "addFF" ++ jmp labelEnd ++
 	  		(label labelEnd) ++ addq (imm 32) !%rsp ++ pushq !%rax ++ pushq !%rbx
	  | Minus -> let (label3, labelEnd) = getIf (), getIf () in
			ins1 ++ ins2 ++ (cmpq (imm nTypeFloat) (ind ~ofs:24 rsp)) ++ je label1 ++
	  		(cmpq (imm nTypeInt) (ind ~ofs:24 rsp)) ++ jne errorTypeE ++
				(cmpq (imm nTypeInt) (ind ~ofs:8 rsp)) ++ jne label2 ++ call "minII" ++ jmp labelEnd ++
				(label label2) ++ (cmpq (imm nTypeFloat) (ind ~ofs:8 rsp)) ++ jne errorTypeE ++ call "minIF" ++ jmp labelEnd ++
 	  		(label label1) ++
				(cmpq (imm nTypeInt) (ind ~ofs:8 rsp)) ++ jne label3 ++ call "minFI" ++ jmp labelEnd ++
				(label label3) ++ (cmpq (imm nTypeFloat) (ind ~ofs:8 rsp)) ++ jne errorTypeE ++ call "minFF" ++ jmp labelEnd ++
 	  		(label labelEnd) ++ addq (imm 32) !%rsp ++ pushq !%rax ++ pushq !%rbx
	  | Times -> let (label3, labelEnd) = getIf (), getIf () in
			ins1 ++ ins2 ++ (cmpq (imm nTypeFloat) (ind ~ofs:24 rsp)) ++ je label1 ++
	  		(cmpq (imm nTypeInt) (ind ~ofs:24 rsp)) ++ jne errorTypeE ++
				(cmpq (imm nTypeInt) (ind ~ofs:8 rsp)) ++ jne label2 ++ call "mulII" ++ jmp labelEnd ++
				(label label2) ++ (cmpq (imm nTypeFloat) (ind ~ofs:8 rsp)) ++ jne errorTypeE ++ call "mulIF" ++ jmp labelEnd ++
 	  		(label label1) ++
				(cmpq (imm nTypeInt) (ind ~ofs:8 rsp)) ++ jne label3 ++ call "mulFI" ++ jmp labelEnd ++
				(label label3) ++ (cmpq (imm nTypeFloat) (ind ~ofs:8 rsp)) ++ jne errorTypeE ++ call "mulFF" ++ jmp labelEnd ++
 	  		(label labelEnd) ++ addq (imm 32) !%rsp ++ pushq !%rax ++ pushq !%rbx
	  | Modulo -> deb ++ (cmpq (imm nTypeInt) !%rax) ++ (jne errorTypeE) ++
	  				(cmpq (imm nTypeInt) !%rcx) ++ (jne errorTypeE) ++
	  				movq !%rdx !%rax ++ movq !%rbx !%rcx ++ xorq !%rdx !%rdx ++
	  				(cmpq (imm 0) !%rax) ++ (movq (imm (-1)) !%r13) ++ (cmovs !%r13 rdx) ++
	  				idivq !%rcx ++ pushq (imm nTypeInt) ++ pushq !%rdx
		| Exp -> let (flottant, fin, label3, label4) = (getIf (), getIf (), getIf (), getIf ()) in
			deb ++ (cmpq (imm nTypeInt) !%rax) ++ (jne errorTypeE) ++
						(cmpq (imm nTypeInt) !%rcx) ++ (jne flottant) ++
						(movq !%rdx !%rax) ++ (movq !%rbx !%rbx) ++ (movq (imm 1) !%rcx) ++
						(label label1) ++
						(cmpq (imm 0) !%rbx) ++ (je label2) ++
						(decq !%rbx) ++ (imulq !%rax !%rcx) ++
						(jmp label1) ++
						(label label2) ++
						(movq (imm nTypeInt) !%rax) ++ (movq !%rcx !%rbx) ++
						(pushq !%rax) ++ (pushq !%rbx) ++ (jmp fin) ++
				(label flottant) ++
					cmpq (imm nTypeFloat) !%rcx ++ jne errorTypeE ++
					movq !%rdx !%xmm0 ++ movq (imm 1) !%rax ++ cvtsi2sdq !%rax xmm1 ++
					(label label3) ++
					(cmpq (imm 0) !%rbx) ++ (je label4) ++
					(decq !%rbx) ++ mulsd !%xmm0 !%xmm1 ++
					(jmp label3) ++
					(label label4) ++
					(movq (imm nTypeFloat) !%rax) ++ (movq !%xmm1 !%rbx) ++
					(pushq !%rax) ++ (pushq !%rbx) ++ label fin
	  | And -> ins1 ++ popq rbx ++ popq rax ++ (cmpq (imm nTypeBool) !%rax) ++ jne errorTypeE ++
	  				(cmpq (imm valTrue) !%rbx) ++ jne label1 ++
	  				ins2 ++ popq rbx ++ popq rax ++ (cmpq (imm nTypeBool) !%rax) ++ jne errorTypeE ++
	  				pushq (imm nTypeBool) ++ pushq !%rbx ++ jmp  label2 ++ (label label1) ++ pushq (imm nTypeBool) ++ pushq (imm valFalse)
	  				++ (label label2)
	  | Or -> ins1 ++ popq rbx ++ popq rax ++ (cmpq (imm nTypeBool) !%rax) ++ jne errorTypeE ++
	  				(cmpq (imm valTrue) !%rbx) ++ je label1 ++
	  				ins2 ++ popq rbx ++ popq rax ++ (cmpq (imm nTypeBool) !%rax) ++ jne errorTypeE ++
	  				pushq (imm nTypeBool) ++ pushq !%rbx ++ jmp  label2 ++ (label label1) ++ pushq (imm nTypeBool) ++ pushq (imm valTrue)
	  				++ (label label2)
	  | Concat -> let label1, label2, label3, label4 = getIf (), getIf (), getIf (), getIf () in
	 		ins1 ++ ins2 ++ popq r15 ++ popq rax ++ popq r14 ++ popq r13 ++ cmpq !%rax !%r13 ++ jne errorTypeE ++ cmpq (imm !nTypeArray) !%rax ++ jl errorTypeE ++
	  		movq (ind ~ofs:8 r14) !%rdi ++ addq (ind ~ofs:8 r15) !%rdi ++ addq (imm 2) !%rdi ++ imulq (imm 8) !%rdi ++ call "malloc" ++
	  		movq !%r13 (ind rax) ++ movq (ind ~ofs:8 r15) !%rdx ++ movq !%rdx (ind ~ofs:8 rax) ++ movq (ind ~ofs:8 r14) !%rbx ++ addq !%rbx (ind ~ofs:8 rax) ++
	  		imulq (imm 8) !%rbx ++ addq (imm 16) !%r14 ++ addq !%r14 !%rbx ++
			leaq (ind ~ofs:16 rax) rcx ++
			jmp label2 ++
			label label1 ++
			movq (ind r14) !%rdx ++ movq !%rdx (ind rcx) ++ addq (imm 8) !%r14 ++ addq (imm 8) !%rcx ++ (* depalcement de l'array 1 vers l'array de sortie et incrementation *)
			label label2 ++
			cmpq !%rbx !%r14 ++
			jne label1 ++
			movq (ind ~ofs:8 r15) !%rbx ++ imulq (imm 8) !%rbx ++ addq (imm 16) !%r15 ++ addq !%r15 !%rbx ++
			jmp label4 ++
			label label3 ++
			movq (ind r15) !%rdx ++ movq !%rdx (ind rcx) ++ addq (imm 8) !%r15 ++ addq (imm 8) !%rcx ++ (* depalcement de l'array 1 vers l'array de sortie et incrementation *)
			label label4 ++
			cmpq !%rbx !%r15 ++
			jne label3 ++
			pushq !%r13 ++ pushq !%rax
		in operation
	| Ident (Tag name) ->
		movq ((if estMac then lab else ilab) ((rectify_character name)^"_type")) !%rax ++ cmpq (imm nTypeUndef) !%rax ++
		je errorUndef ++ movq ((if estMac then lab else ilab) ((rectify_character name)^"_val")) !%rbx ++
		pushq !%rax ++ pushq !%rbx
	| Ident (Dec offset) ->
		movq (ind ~ofs:(offset+8) rbp) !%rax ++ cmpq (imm nTypeUndef) !%rax ++
		je errorUndef ++ pushq !%rax ++ pushq (ind ~ofs:offset rbp)
	| Index (exp, ident, offset) ->
		let numClasse = numStruct ident in
		(compile_expr exp) ++ (popq rbx) ++ (popq rax) ++ (cmpq (imm numClasse) !%rax) ++ (jne errorTypeE) ++
		(movq (ind ~ofs:(offset + 0) rbx) !%rax) ++ (movq (ind ~ofs:(offset + 8) rbx) !%rbx) ++ pushq !%rax ++ pushq !%rbx
	| Array (e1, e2) ->
		let label_get_element = getIf () in
		compile_expr e1 ++
		compile_expr e2 ++
		popq rdx ++ popq rcx ++ (* type et valeur de l'indice *)
		popq rbx ++ popq rax ++ (* type et valeur du pointeur vers le début *)

		cmpq (imm !nTypeArray) !%rax ++ jl errorTypeE ++ (* On vérifie que c'est bien un array*)
		
		cmpq (imm nTypeInt) !%rcx ++ jne errorTypeE ++ (* On vérifie que l'indice est bien un entier *)

		cmpq (imm 0) !%rdx ++ jge label_get_element ++ (* Si l'indice est négatif *)
		addq (ind ~ofs:8 rbx) !%rdx ++
		label label_get_element ++

		cmpq (ind ~ofs:8 rbx) !%rdx ++ jge errorOoB ++ (* On compare qu'on reste dans les bornes de l'array ;) *) (* TODO vérifier que c'est bon *)
		
		movq (ind ~ofs:0 rbx) !%rax ++ (* Acquisition du type *)
		subq (imm !nTypeArray) !%rax ++
		movq (ind ~ofs:16 ~index:rdx ~scale:8 rbx) !%rbx ++ (* Acquisition de la valeur *)
		pushq !%rax ++ pushq !%rbx (* On empile le résultat *)
	| LvalueAffectV (Tag name, expr) ->
		let code = compile_expr expr in
		code ++ (popq rbx) ++ (popq rax) ++ movq !%rbx (univerlab ((rectify_character name)^"_val")) ++ movq !%rax (univerlab ((rectify_character name)^"_type")) ++ pushq !%rax ++ pushq !%rbx
	| LvalueAffectV (Dec offset, expr) ->
		let code = compile_expr expr in
		code ++ (popq rbx) ++ (popq rax) ++ (movq !%rax (ind ~ofs:(offset+8) rbp)) ++ (movq !%rbx (ind ~ofs:offset rbp)) ++ pushq !%rbx ++ pushq !%rax
	| LvalueAffectI (exp1, ident, entier, exp2) ->
		let code1 = compile_expr exp2 in (* J'ai changé code1 et code2 :D *)
		let code2 = compile_expr exp1 in
		let (field_map, numero) = Tmap.find ident !structMap in (* numero est le code de type de la structure - 5*)
		let numeroBis = numero + nTypeStruct in
		let (cle, (field_index, field_type)) = Tmap.find_first (fun cle -> let (num, _) = Tmap.find cle field_map in num = numero) field_map in
		let comparaison = (popq r14) ++ (popq rax) ++ (cmpq (imm numeroBis) !%rax) ++ (jne errorTypeE) in
		let target_type = (popq rbx) ++ (popq rax) ++
			(if field_type != Any then (cmpq (ind ~ofs:(entier + 0) r14) !%rax) ++ (jne errorTypeE) else nop) ++(* vérification de type qu'on met dans le champ *)
			(movq !%rax (ind ~ofs:(entier + 0) r14)) ++
			(movq !%rbx (ind ~ofs:(entier + 8) r14)) in
		code1 ++ code2 ++ comparaison ++ target_type ++ pushq !%rax ++ pushq !%rbx
	| LvalueAffectA (e1, e2, e3) -> 
		let label_get_element = getIf () in
		compile_expr e1 ++
		compile_expr e2 ++
		compile_expr e3 ++
		popq r15 ++ popq r13 ++ (* type et valeur à insérer*)
		popq rdx ++ popq rcx ++ (* type et valeur de l'indice *)
		popq rbx ++ popq rax ++ (* type et valeur de la donnée (array, a priori) à affecter *)
		addq (imm !nTypeArray) !%r13 ++

		cmpq (imm !nTypeArray) !%rax ++ jl errorTypeE ++ (* On vérifie que c'est bien un array *)
		cmpq (imm nTypeInt) !%rcx ++ jne errorTypeE ++ (* On vérifie que l'indice est bien un entier *)

		cmpq (imm 0) !%rdx ++ jge label_get_element ++ (* Si l'indice est négatif *)
		addq (ind ~ofs:8 rbx) !%rdx ++
		label label_get_element ++

		cmpq (ind ~ofs:8 rbx) !%rdx ++ jge errorOoB ++ (* On compare qu'on reste dans les bornes de l'array ;) *)


		cmpq (ind ~ofs:0 rbx) !%r13 ++ jne errorTypeE ++ (* On vérifie que la valeur insérée a le même type que le reste *)
		
		
		imulq (imm 1) !%rbx ++
		movq !%r15 (ind ~ofs:16 ~index:rdx ~scale:8 rbx) ++(* Insertion de la valeur *) 
		pushq (imm nTypeNothing) ++ pushq (imm nTypeBool)
	| Ret (pjtype, exp) ->
		compile_expr exp ++ (popq rbx) ++ (popq rax) ++
		(if pjtype = Any then nop else (cmpq (imm (int_of_type pjtype)) !%rax ++
		jne errorTypeE)) ++ movq !%rbp !%rsp ++ popq rbp ++ ret
	| For (posC, posFLoc, exp1, exp2, bloc) ->
		let undef = ref nop in
		let n1 = posFLoc/16 in
		let n2 = posC/16 in
		let () = for i = n1+1 to n2-1 do
			undef := !undef ++ movq (imm nTypeUndef) (ind ~ofs:(i*16+8) rbp)
		done in
		let lDeb = getFor () in
		let lFin = getFor () in
		let e1 = compile_expr exp1 in
		let e2 = compile_expr exp2 in
		let b = compile_bloc bloc in
		e1 ++ e2 ++ movq (imm nTypeInt) (ind ~ofs:(posC+8) rbp) ++
		(popq rdx) ++ (popq rcx) ++ (popq rbx) ++ (popq rax) ++
		cmpq (imm nTypeInt) !%rcx ++ jne errorTypeE ++
		cmpq (imm nTypeInt) !%rax ++ jne errorTypeE ++
		decq !%rbx ++
		pushq !%rbx ++ pushq !%rdx ++
		jmp lFin ++
		label lDeb ++
		!undef ++
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
		let undef = ref nop in
		let n1 = finLoc/16 in
		let n2 = debLoc/16 in
		let () = for i = n1 to n2-1 do
			undef := !undef ++ movq (imm nTypeUndef) (ind ~ofs:(i*16+8) rbp)
		done in
		let e = compile_expr exp in
		let b = compile_bloc bloc in
		let (label1, label2) = (getWhile (), getWhile ()) in
		let comp = (label label1) ++ e ++ (popq rbx) ++ (popq rax) ++ (cmpq (imm (nTypeBool)) !%rax) ++ (jne errorTypeE) ++ (cmpq (imm valTrue) !%rbx) ++ (jne label2) in
		let corps = b ++ popn 16 ++ (jmp label1) ++ (label label2) in
		comp ++ !undef ++ corps ++ pushq (imm nTypeNothing) ++ pushq !%rax
	| DoWhile (debLoc, finLoc, bloc) ->
		let undef = ref nop in
		let n1 = finLoc/16 in
		let n2 = debLoc/16 in
		let () = for i = n1 to n2-1 do
			undef := !undef ++ movq (imm nTypeUndef) (ind ~ofs:(i*16+8) rbp)
		done in
		let corps = compile_bloc bloc in
		let label1 = getWhile () in
		let comp = (popq rbx) ++ (popq rax) ++ (cmpq (imm (nTypeBool)) !%rax) ++ (jne errorTypeE) ++ (cmpq (imm valTrue) !%rbx) ++ (je label1) in
		(label label1) ++ !undef ++ corps ++ comp ++ pushq (imm nTypeNothing) ++ pushq !%rax
	| If (exp, bloc, else_) ->
		let c = compile_expr exp in
		let c1 = compile_bloc bloc in
		let c2 = compile_else_ else_ in
		let (label1, label2) = (getIf (), getIf ()) in
		c ++ (popq rbx) ++ (popq rax) ++ (cmpq (imm nTypeBool) !%rax) ++ (jne errorTypeE) ++
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
		label (rectify_character f) ++
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
    label ((rectify_character n)^"_"^string_of_int i)++
    pushq !%rbp ++ movq !%rsp !%rbp ++
    pushn (-o2) ++
    code ++
    (if rT = Any then nop else (cmpq (imm (int_of_type rT)) !%rax ++ jne errorTypeE)) ++
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
    label ((rectify_character n)^"_"^string_of_int i) ++
    pushq !%rbp ++ movq !%rsp !%rbp ++
		movq (imm (16*longueur)) !%rdi ++ call "malloc" ++ !code ++
    movq !%rax !%rbx ++ movq (imm nType) !%rax ++
    movq !%rbp !%rsp ++ popq rbp ++
    ret

let compile_program f ofile =
 let (eL, i, smap, fmap) = alloc_fichier f in (* smap est le map des variables globales *)
(* print_int i;
 print_newline ();*)
 let code = List.fold_left (fun d e -> (if d!=nop then d ++ popn 16 else nop) ++ compile_expr e) nop eL in
 let codefun = Tmap.fold (fun k imap asm -> Imap.fold (fun i f asm2 -> asm2 ++ compile_fun k i f) imap asm) fmap nop in
 let deplq = if estMac then (fun x y -> leaq x y) else (fun x y -> movq x !%y) in (* remplacement de rdi par y*)
 let p =
   { text =
		globl "main" ++ label "main" ++
		pushq !%rbx ++ pushq !%r12 ++
		pushq !%rbp ++ movq !%rsp !%r12 ++
		movq !%rsp !%rbp ++
		pushn (-i) ++
		label "begin" ++
		code ++
		movq !%rbp !%rsp ++
		popq rbp ++
		popq r12 ++ popq rbx ++
		movq (imm 0) !%rax ++ (* exit *)
		ret ++

	(if !CompilRef.l then
		label "loII" ++
		movq (ind ~ofs:8 rsp) !%rbx ++
		movq (ind ~ofs:24 rsp) !%rax ++
		cmpq !%rax !%rbx ++
		jg "loII_f" ++
		movq (imm valFalse) !%rbx ++
		jmp "loII_t" ++
		label "loII_f" ++
		movq (imm valTrue) !%rbx ++
		label "loII_t" ++
		movq (imm nTypeBool) !%rax ++
		ret ++

		label "loFI" ++
		movq (ind ~ofs:8 rsp) !%rbx ++ 
		movq (ind ~ofs:24 rsp) !%xmm0 ++
		cvtsi2sdq !%rax xmm1 ++ 
		cmpltsd !%xmm1 !%xmm0 ++ 
		movq !%xmm0 !%rbx ++
		movq (imm nTypeBool) !%rax ++
		cmpq (imm (-1)) !%rbx ++
		jne "loFI_f" ++
		movq (imm valTrue) !%rbx ++
		jmp "loFI_t" ++
		label "loFI_f" ++
		movq (imm valFalse) !%rbx ++
		label "loFI_t" ++
		ret ++

		label "loIF" ++
		movq (ind ~ofs:8 rsp) !%xmm0 ++
		movq (ind ~ofs:24 rsp) !%rbx ++
		cvtsi2sdq !%rbx xmm1 ++
		cmpltsd !%xmm0 !%xmm1 ++ 
		movq !%xmm1 !%rbx ++
		movq (imm nTypeBool) !%rax ++
		cmpq (imm (-1)) !%rbx ++
		jne "loIF_f" ++
		movq (imm valTrue) !%rbx ++
		jmp "loIF_t" ++
		label "loIF_f" ++
		movq (imm valFalse) !%rbx ++
		label "loIF_t" ++
		ret ++

		label "loFF" ++
		movq (ind ~ofs:8 rsp) !%xmm0 ++
		movq (ind ~ofs:24 rsp) !%xmm1 ++
		cmpltsd !%xmm0 !%xmm1 ++
		movq !%xmm1 !%rbx ++
		cmpq (imm (-1)) !%rbx ++
		jne "loFF_f" ++
		movq (imm valTrue) !%rbx ++
		jmp "loFF_t" ++
		label "loFF_f" ++
		movq (imm valFalse) !%rbx ++
		label "loFF_t" ++
		movq (ind ~ofs:24 rsp) !%rax ++
		movq (imm nTypeBool) !%rax ++
		ret
		else nop) ++

	(if !CompilRef.g then
		label "goII" ++
		movq (ind ~ofs:8 rsp) !%rbx ++
		movq (ind ~ofs:24 rsp) !%rax ++
		cmpq !%rax !%rbx ++
		jl "goII_f" ++
		movq (imm valFalse) !%rbx ++
		jmp "goII_t" ++
		label "goII_f" ++
		movq (imm valTrue) !%rbx ++
		label "goII_t" ++
		movq (imm nTypeBool) !%rax ++
		ret ++

		label "goFI" ++
		movq (ind ~ofs:8 rsp) !%rbx ++ 
		movq (ind ~ofs:24 rsp) !%xmm0 ++
		cvtsi2sdq !%rax xmm1 ++ 
		cmpnlesd !%xmm1 !%xmm0 ++ 
		movq !%xmm0 !%rbx ++
		movq (imm nTypeBool) !%rax ++
		cmpq (imm (-1)) !%rbx ++
		jne "goFI_f" ++
		movq (imm valTrue) !%rbx ++
		jmp "goFI_t" ++
		label "goFI_f" ++
		movq (imm valFalse) !%rbx ++
		label "goFI_t" ++
		ret ++

		label "goIF" ++
		movq (ind ~ofs:8 rsp) !%xmm0 ++
		movq (ind ~ofs:24 rsp) !%rbx ++
		cvtsi2sdq !%rbx xmm1 ++
		cmpnlesd !%xmm0 !%xmm1 ++ 
		movq !%xmm1 !%rbx ++
		movq (imm nTypeBool) !%rax ++
		cmpq (imm (-1)) !%rbx ++
		jne "goIF_f" ++
		movq (imm valTrue) !%rbx ++
		jmp "goIF_t" ++
		label "goIF_f" ++
		movq (imm valFalse) !%rbx ++
		label "goIF_t" ++
		ret ++

		label "goFF" ++
		movq (ind ~ofs:8 rsp) !%xmm0 ++
		movq (ind ~ofs:24 rsp) !%xmm1 ++
		cmpnlesd !%xmm0 !%xmm1 ++
		movq !%xmm1 !%rbx ++
		cmpq (imm (-1)) !%rbx ++
		jne "goFF_f" ++
		movq (imm valTrue) !%rbx ++
		jmp "goFF_t" ++
		label "goFF_f" ++
		movq (imm valFalse) !%rbx ++
		label "goFF_t" ++
		movq (ind ~ofs:24 rsp) !%rax ++
		movq (imm nTypeBool) !%rax ++
		ret 
		else nop) ++

	(if !CompilRef.le then
		label "leqII" ++
		movq (ind ~ofs:8 rsp) !%rbx ++
		movq (ind ~ofs:24 rsp) !%rax ++
		cmpq !%rax !%rbx ++
		jge "leqII_f" ++
		movq (imm valFalse) !%rbx ++
		jmp "leqII_t" ++
		label "leqII_f" ++
		movq (imm valTrue) !%rbx ++
		label "leqII_t" ++
		movq (imm nTypeBool) !%rax ++
		ret ++

		label "leqFI" ++
		movq (ind ~ofs:8 rsp) !%rbx ++ 
		movq (ind ~ofs:24 rsp) !%xmm0 ++
		cvtsi2sdq !%rax xmm1 ++ 
		cmplesd !%xmm1 !%xmm0 ++ 
		movq !%xmm0 !%rbx ++
		movq (imm nTypeBool) !%rax ++
		cmpq (imm (-1)) !%rbx ++
		jne "leqFI_f" ++
		movq (imm valTrue) !%rbx ++
		jmp "leqFI_t" ++
		label "leqFI_f" ++
		movq (imm valFalse) !%rbx ++
		label "leqFI_t" ++
		ret ++

		label "leqIF" ++
		movq (ind ~ofs:8 rsp) !%xmm0 ++
		movq (ind ~ofs:24 rsp) !%rbx ++
		cvtsi2sdq !%rbx xmm1 ++
		cmplesd !%xmm0 !%xmm1 ++ 
		movq !%xmm1 !%rbx ++
		movq (imm nTypeBool) !%rax ++
		cmpq (imm (-1)) !%rbx ++
		jne "leqIF_f" ++
		movq (imm valTrue) !%rbx ++
		jmp "leqIF_t" ++
		label "leqIF_f" ++
		movq (imm valFalse) !%rbx ++
		label "leqIF_t" ++
		ret ++

		label "leqFF" ++
		movq (ind ~ofs:8 rsp) !%xmm0 ++
		movq (ind ~ofs:24 rsp) !%xmm1 ++
		cmplesd !%xmm0 !%xmm1 ++
		movq (imm nTypeFloat) !%rax ++
		movq !%xmm1 !%rbx ++
		cmpq (imm (-1)) !%rbx ++
		jne "leqFF_f" ++
		movq (imm valTrue) !%rbx ++
		jmp "leqFF_t" ++
		label "leqFF_f" ++
		movq (imm valFalse) !%rbx ++
		label "leqFF_t" ++
		movq (ind ~ofs:24 rsp) !%rax ++
		movq (imm nTypeBool) !%rax ++
		ret 
		else nop) ++

	(if !CompilRef.ge then
		label "geqII" ++
		movq (ind ~ofs:8 rsp) !%rbx ++
		movq (ind ~ofs:24 rsp) !%rax ++
		cmpq !%rax !%rbx ++
		jle "geqII_f" ++
		movq (imm valFalse) !%rbx ++
		jmp "geqII_t" ++
		label "geqII_f" ++
		movq (imm valTrue) !%rbx ++
		label "geqII_t" ++
		movq (imm nTypeBool) !%rax ++
		ret ++

		label "geqFI" ++
		movq (ind ~ofs:8 rsp) !%rbx ++ 
		movq (ind ~ofs:24 rsp) !%xmm0 ++
		cvtsi2sdq !%rax xmm1 ++ 
		cmpnltsd !%xmm1 !%xmm0 ++ 
		movq !%xmm0 !%rbx ++
		movq (imm nTypeBool) !%rax ++
		cmpq (imm (-1)) !%rbx ++
		jne "geqFI_f" ++
		movq (imm valTrue) !%rbx ++
		jmp "geqFI_t" ++
		label "geqFI_f" ++
		movq (imm valFalse) !%rbx ++
		label "geqFI_t" ++
		ret ++

		label "geqIF" ++
		movq (ind ~ofs:8 rsp) !%xmm0 ++
		movq (ind ~ofs:24 rsp) !%rbx ++
		cvtsi2sdq !%rbx xmm1 ++
		cmpnltsd !%xmm0 !%xmm1 ++ 
		movq !%xmm1 !%rbx ++
		movq (imm nTypeBool) !%rax ++
		cmpq (imm (-1)) !%rbx ++
		jne "geqIF_f" ++
		movq (imm valTrue) !%rbx ++
		jmp "geqIF_t" ++
		label "geqIF_f" ++
		movq (imm valFalse) !%rbx ++
		label "geqIF_t" ++
		ret ++

		label "geqFF" ++
		movq (ind ~ofs:8 rsp) !%xmm0 ++
		movq (ind ~ofs:24 rsp) !%xmm1 ++
		cmpnltsd !%xmm0 !%xmm1 ++
		movq (imm nTypeFloat) !%rax ++
		movq !%xmm1 !%rbx ++
		cmpq (imm (-1)) !%rbx ++
		jne "geqFF_f" ++
		movq (imm valTrue) !%rbx ++
		jmp "geqFF_t" ++
		label "geqFF_f" ++
		movq (imm valFalse) !%rbx ++
		label "geqFF_t" ++
		movq (ind ~ofs:24 rsp) !%rax ++
		movq (imm nTypeBool) !%rax ++
		ret 
		else nop) ++

	(if !CompilRef.add then
		label "addII" ++
		movq (ind ~ofs:8 rsp) !%rbx ++
		movq (ind ~ofs:24 rsp) !%rax ++
		addq !%rax !%rbx ++
		movq (imm nTypeInt) !%rax ++
		ret ++
		
		label "addFI" ++
		movq (ind ~ofs:8 rsp) !%rbx ++
		movq (ind ~ofs:24 rsp) !%xmm0 ++
		cvtsi2sdq !%rax xmm1 ++
		addsd !%xmm1 !%xmm0 ++
		movq (imm nTypeFloat) !%rax ++
		movq !%xmm0 !%rbx ++
		ret ++
		
		label "addIF" ++
		movq (ind ~ofs:8 rsp) !%xmm0 ++
		movq (ind ~ofs:24 rsp) !%rbx ++
		cvtsi2sdq !%rbx xmm1 ++
		addsd !%xmm1 !%xmm0 ++
		movq (imm nTypeFloat) !%rax ++
		movq !%xmm0 !%rbx ++
		ret ++
	
		label "addFF" ++
		movq (ind ~ofs:8 rsp) !%xmm0 ++
		movq (ind ~ofs:24 rsp) !%xmm1 ++
		addsd !%xmm1 !%xmm0 ++
		movq (imm nTypeFloat) !%rax ++
		movq !%xmm0 !%rbx ++
		ret 
		else nop) ++

	(if !CompilRef.mul then
		label "mulII" ++
		movq (ind ~ofs:8 rsp) !%rbx ++
		movq (ind ~ofs:24 rsp) !%rax ++
		imulq !%rax !%rbx ++
		movq (imm nTypeInt) !%rax ++
		ret ++
		label "mulFI" ++
		movq (ind ~ofs:8 rsp) !%rbx ++
		movq (ind ~ofs:24 rsp) !%xmm0 ++
		cvtsi2sdq !%rax xmm1 ++
		mulsd !%xmm1 !%xmm0 ++
		movq (imm nTypeFloat) !%rax ++
		movq !%xmm0 !%rbx ++
		ret ++
		label "mulIF" ++
		movq (ind ~ofs:8 rsp) !%xmm0 ++
		movq (ind ~ofs:24 rsp) !%rbx ++
		cvtsi2sdq !%rbx xmm1 ++
		mulsd !%xmm1 !%xmm0 ++
		movq (imm nTypeFloat) !%rax ++
		movq !%xmm0 !%rbx ++
		ret ++
		label "mulFF" ++
		movq (ind ~ofs:8 rsp) !%xmm0 ++
		movq (ind ~ofs:24 rsp) !%xmm1 ++
		mulsd !%xmm1 !%xmm0 ++
		movq (imm nTypeFloat) !%rax ++
		movq !%xmm0 !%rbx ++
		ret 
		else nop) ++

	(if !CompilRef.sub then
		label "minII" ++
		movq (ind ~ofs:8 rsp) !%rax ++
		movq (ind ~ofs:24 rsp) !%rbx ++
		subq !%rax !%rbx ++
		movq (imm nTypeInt) !%rax ++
		ret ++
		label "minFI" ++
		movq (ind ~ofs:8 rsp) !%rbx ++
		movq (ind ~ofs:24 rsp) !%xmm0 ++
		cvtsi2sdq !%rax xmm1 ++
		subsd !%xmm1 !%xmm0 ++
		movq (imm nTypeFloat) !%rax ++
		movq !%xmm0 !%rbx ++
		ret ++
		label "minIF" ++
		movq (ind ~ofs:8 rsp) !%xmm1 ++
		movq (ind ~ofs:24 rsp) !%rbx ++
		cvtsi2sdq !%rbx xmm0 ++
		subsd !%xmm1 !%xmm0 ++
		movq (imm nTypeFloat) !%rax ++
		movq !%xmm0 !%rbx ++
		ret ++
		label "minFF" ++
		movq (ind ~ofs:8 rsp) !%xmm1 ++
		movq (ind ~ofs:24 rsp) !%xmm0 ++
		subsd !%xmm1 !%xmm0 ++
		movq (imm nTypeFloat) !%rax ++
		movq !%xmm0 !%rbx ++
		ret 
		else nop) ++

	(if !CompilRef.div then
		label "div_0" ++
		pushq !%rax ++
		movq (ind ~ofs:16 rsp) !%rcx ++
		cmpq (imm 0) !%rcx ++
		je error0div ++
		movq (ind ~ofs:32 rsp) !%rax ++
	  	xorq !%rdx !%rdx ++
	  	(cmpq (imm 0) !%rax) ++
	  	(movq (imm (-1)) !%r13) ++
	  	(cmovs !%r13 rdx) ++
		(cmpq (imm 0) !%rcx) ++ (je error0div) ++
		idivq !%rcx ++
		movq !%rax !%rbx ++
		movq (imm nTypeInt) !%rax ++
		popq rcx ++
		ret ++
	
		label "div_1" ++
		pushq !%rax ++
		movq (ind ~ofs:16 rsp) !%rbx ++
		cmpq (imm 0) !%rbx ++
		je error0div ++
		movq (ind ~ofs:32 rsp) !%xmm0 ++
		cvtsi2sdq !%rax xmm1 ++
		divsd !%xmm1 !%xmm0 ++
		movq (imm nTypeFloat) !%rax ++
		movq !%xmm0 !%rbx ++
		popq rcx ++
		ret ++
	
		label "div_2" ++
		pushq !%rax ++
		movq (ind ~ofs:16 rsp) !%xmm1 ++
		xorq !%rax !%rax ++
		cvtsi2sdq !%rax xmm0 ++
		ucomisd !%xmm0 !%xmm1 ++
		je error0div ++
		movq (ind ~ofs:32 rsp) !%rbx ++
		cvtsi2sdq !%rbx xmm0 ++
		divsd !%xmm1 !%xmm0 ++
		movq (imm nTypeFloat) !%rax ++
		movq !%xmm0 !%rbx ++
		popq rcx ++
		ret ++
	
		label "div_3" ++
		pushq !%rax ++
		movq (ind ~ofs:16 rsp) !%xmm1 ++
		xorq !%rax !%rax ++
		cvtsi2sdq !%rax xmm0 ++
		ucomisd !%xmm0 !%xmm1 ++
		je error0div ++
		movq (ind ~ofs:32 rsp) !%xmm0 ++
		divsd !%xmm1 !%xmm0 ++
		movq (imm nTypeFloat) !%rax ++
		movq !%xmm0 !%rbx ++
		popq rcx ++
		ret 
		else nop) ++

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
		(cmpq (imm (nTypeChar + !nTypeArray)) !%rax) ++
		je "ifString" ++
		(cmpq (imm (nTypeChar)) !%rax) ++
		je "ifChar" ++
		ret ++
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
		label "ifChar" ++
		call "print_char" ++
		ret ++

	label "print_int" ++
		movq !%rdi !%rsi ++
		deplq (lab ".Sprint_int") rdi ++
		movq (imm 0) !%rax ++
		call "printf" ++
    	ret ++

	label "print_float" ++
		movq !%rdi !%xmm0 ++
		deplq (lab ".Sprint_float") rdi ++
		movq (imm 1) !%rax ++
		call "printf" ++
		movq (imm 0) !%rax ++
			ret ++
	
	label "print_char" ++
		movq !%rdi !%rsi ++
		deplq (lab ".Sprint_char") rdi ++
		movq (imm 0) !%rax ++
		call "printf" ++
		movq (imm 0) !%rax ++
			ret ++

	label "print_string" ++
		movq !%rdi !%rbx ++
		movq (ind ~ofs:8 rbx) !%r14 ++
		imulq (imm 8) !%r14 ++
		addq (imm 16) !%rbx ++
		addq !%rbx !%r14 ++
		jmp "print_string_end" ++
		label "print_string_begin" ++
		movq (ind rbx) !%rsi ++
		deplq (lab ".Sprint_char") rdi ++
		call "printf" ++
		addq (imm 8) !%rbx ++
		label "print_string_end" ++
		cmpq !%rbx !%r14 ++
		jne "print_string_begin" ++
    	ret ++

	label "print_bool" ++
		movq !%rdi !%rsi ++
		cmpq (imm valFalse) !%rsi ++
		je "print_false" ++
		deplq (lab ".Sprint_true") rdi ++
		jmp "print_end" ++
		label "print_false" ++
		deplq (lab ".Sprint_false") rdi ++
		label "print_end" ++
		movq (imm 0) !%rax ++
		call "printf" ++
    	ret ++

    label "array_length_0" ++
    	movq (ind ~ofs:8 rsp) !%rax ++
		movq (ind ~ofs:8 rax) !%rbx ++
		movq (imm nTypeInt) !%rax ++
		ret ++

	(if !CompilRef.int then
		label "int_0" ++
		movq (ind ~ofs:8 rsp) !%xmm0 ++
		cvttsd2siq !%xmm0 rbx ++
		movq (imm nTypeInt) !%rax ++
		ret ++

		label "int_1" ++
		movq (ind ~ofs:8 rsp) !%rbx ++
		movq (imm nTypeInt) !%rax ++
		ret ++

		label "int_2" ++
		movq (ind ~ofs:8 rsp) !%rbx ++
		movq (imm nTypeInt) !%rax ++
		ret
		else nop) ++

	(if !CompilRef.float then
		label "float_0" ++
		movq (ind ~ofs:8 rsp) !%rax ++
		cvtsi2sdq !%rax xmm0 ++
		movq !%xmm0 !%rbx ++
		movq (imm nTypeFloat) !%rax ++
		ret ++
	
		label "float_1" ++
		movq (ind ~ofs:8 rsp) !%rbx ++
		movq (imm nTypeFloat) !%rax ++
		ret 
		else nop) ++ 

	(if !CompilRef.char then
		label "char_0" ++
		movq (ind ~ofs:8 rsp) !%rbx ++
		movq (imm nTypeChar) !%rax ++
		ret
		else nop) ++

	label "sqrt_0" ++
		movq (ind ~ofs:8 rsp) !%rax ++
		cvtsi2sdq !%rax xmm0 ++
		sqrtsd !%xmm0 !%xmm0 ++
		movq !%xmm0 !%rbx ++
		movq (imm nTypeFloat) !%rax ++
		ret ++

	label "sqrt_1" ++
		movq (ind ~ofs:8 rsp) !%xmm0 ++
		sqrtsd !%xmm0 !%xmm0 ++
		movq !%xmm0 !%rbx ++
		movq (imm nTypeFloat) !%rax ++
		ret ++

	(if !CompilRef.input_int then
		label "input_int_0" ++
		pushq !%rbp ++
		deplq (lab ".Sprint_int") rdi ++
		deplq (lab ".Sscan_int") rsi ++
		call "scanf" ++
		movq ((if estMac then lab else ilab) (".Sscan_int")) !%rbx ++
		movq (imm nTypeInt) !%rax ++
		popq rbp ++
		ret
		else nop) ++
	
	label "delay_0" ++
		movq (ind ~ofs:8 rsp) !%rbx ++
		movq (ind ~ofs:16 rsp) !%rax ++
		cmpq (imm nTypeInt) !%rax ++ jne errorTypeE ++ (* delay prend en entrée un nombre entier -> les milisecondes *)
		subq (imm 16) !%rsp ++
		movq (imm 0) (ind ~ofs:8 rsp) ++
		movq !%rbx (ind ~ofs:0 rsp) ++
		movq !%rsp !%rdi ++
		movq (imm 0) !%rsi ++
		movq (imm 35) !%rax ++
		syscall () ++
		addq (imm 16) !%rsp ++
		ret ++
	
	label "timestamp_0" ++
		xorq !%rax !%rax ++
		rdtsc () ++ shlq (imm 32) !%rdx ++
		addq !%rdx !%rax ++ movq !%rax !%rbx ++
		movq (imm nTypeInt) !%rax ++
		ret ++
	
	label "typeof_0" ++
		movq !%rax !%rbx ++
		movq (imm nTypeInt) !%rax ++
		ret ++

	label errorAssert ++
		movq !%rdi !%rdx ++
		deplq (lab ".Sprint_assert") rdi ++
		call "printf" ++
		movq !%r12 !%rsp ++
		popq rbp ++
		popq r12 ++ popq rbx ++
		movq (imm 1) !%rax ++
		ret ++

	label error0div ++
		deplq (lab ".Sprint_0div") rdi ++
		call "printf" ++
		movq !%r12 !%rsp ++
		popq rbp ++
		popq r12 ++ popq rbx ++
		movq (imm 1) !%rax ++
		ret ++ 
	
	label errorCall1 ++
		deplq (lab ".Sprint_call1") rdi ++
		call "printf" ++
		movq !%r12 !%rsp ++
		popq rbp ++
		popq r12 ++ popq rbx ++
		movq (imm 1) !%rax ++
		ret ++ 
	
	label errorCall2 ++
		deplq (lab ".Sprint_call2") rdi ++
		call "printf" ++
		movq !%r12 !%rsp ++
		popq rbp ++
		popq r12 ++ popq rbx ++
		movq (imm 1) !%rax ++
		ret ++ 
	
	label errorOoB ++
		deplq (lab ".Sprint_IOoB") rdi ++ 
		call "printf" ++
		movq !%r12 !%rsp ++
		popq rbp ++
		popq r12 ++ popq rbx ++
		movq (imm 1) !%rax ++
		ret ++

	label errorUndef ++
		deplq (lab ".Sprint_undef") rdi ++
		call "printf" ++
		movq !%r12 !%rsp ++
		popq rbp ++
		popq r12 ++ popq rbx ++
		movq (imm 1) !%rax ++
		ret ++ 

	label errorTypeE ++
		deplq (lab ".Sprint_typing") rdi ++
		call "printf" ++
		movq !%r12 !%rsp ++
		popq rbp ++
		popq r12 ++ popq rbx ++
		movq (imm 1) !%rax ++
		ret ++
	
	codefun;

    data =
       Hashtbl.fold (fun x i l -> l ++ label ("string"^string_of_int i) ++ string x) sMap nop ++
			 Hashtbl.fold (fun x i l -> l ++ label ("constant_float"^string_of_int i) ++ (double (float_of_string x))) fMap nop ++
			 Tmap.fold (fun x i l -> if x<> "nothing" then l ++ label ((rectify_character x)^"_type") ++ (dquad [nTypeUndef])
			 														++ label ((rectify_character x)^"_val") ++ (dquad [0]) else l) smap nop ++
		(label "nothing_type" ++ (dquad [nTypeNothing])) ++
		(label "nothing_val" ++ (dquad [0])) ++
        (label ".Sprint_int" ++ string "%zd") ++
		(label ".Sprint_float" ++ string "%f") ++
		(label ".Sprint_char" ++ string "%c") ++
		(label ".Sprint_true" ++ string "true") ++
		(label ".Sprint_false" ++ string "false") ++
(*		(label ".Sprint_error" ++ string "Type failure\n") ++*)
		(label ".Sprint_0div" ++ string "Division by zero error\n") ++
		(label ".Sprint_IOoB" ++ string "Index out of bound error\n") ++
		(label ".Sprint_undef" ++ string "Undef error\n") ++
		(label ".Sprint_typing" ++ string "Wrong type error\n") ++
		(label ".Sprint_call1" ++ string "No compatible function error\n") ++
		(label ".Sprint_call2" ++ string "Too many compatible functions error\n") ++
		(label ".Sprint_assert" ++ string "Assertion error in file %s on line %d\n") ++
		(label ".Sscan_int" ++ (dquad [0])) ++ 
		(label ".Sscan_string" ++ (dquad [0]))
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
