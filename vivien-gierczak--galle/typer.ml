open Astype
open Ast
(*
#########################################
#                                       #
#        Typeur de Petit-Julia          #
#                                       #
# utilisé par le compilateur et le REPL #
#                                       #
#########################################

*)

(* variable pour savoir si on est dans l'interpréteur ou dans le compilateur *)
let estCompile = ref false

(* teste si deux types sont compatibles *)
let compatible (t1:Astype.pjtype) (t2:Astype.pjtype) = t1 = Any || t2 = Any || t1 = t2

let rec rajouteFonction (i1, n1, l1) = function
  |[] -> [ISet.singleton i1, n1, l1]
  |(i2, n2, l2)::tl when l2=l1 ->
    if n1<n2 then (ISet.singleton i1, n1, l1)::tl
    else if n1 > n2 then (i2, n2, l2)::tl
    else (ISet.add i1 i2, n2, l2)::tl
  |(i2, n2, l2)::tl -> (i2, n2, l2)::rajouteFonction (i1, n1, l1) tl

(* Teste si f est identiques à toutes les autres fonctions dans la liste *)
let rec compatibleFInL ((n1,l1) as f) = function
  |[] -> true
  |(n2,l2)::tl -> n1==n2 && l1==l2 && compatibleFInL f tl

(* lanceur d'erreur *)
let error (msg:string) (p:Ast.position) = raise (Ast.Typing_Error_Msg_Pos (msg,p) )

(* teste si le type t existe bien *)
let exists (t:Astype.pjtype) (env:structEnv):bool = match t with
  | Any | Nothing | Int64 | Float64 | Bool | Char64 -> true
  | S s -> Tmap.mem s env
  | Array -> true

(* teste si le champ existe *)
let argExists (t:string) (env:argsEnv) = Tmap.mem t env

(* convertie le type en une string pour les messages d'erreur *)
let typeName (t:Astype.pjtype):string = match t with
  | Int64 -> "Int64"
  | Float64 -> "Float64"
  | Nothing -> "Nothing"
  | Bool -> "Bool"
  | S s -> "Struct \""^s^"\""
  | Array -> "Array"
  | Any -> "Any"
  | Char64 -> "Char64"

(* teste la correction d'une déclaration de structure et la rajoute aux différents environnements *)
let parcoursStruct (sE:structEnv) (aE:argsEnv) (fE:funcEnv) (b,p,str,l):(structEnv * argsEnv * funcEnv) =
  if str = "print" || str = "println" || str = "div" then
    error (str^" is not an allowed structuture name") p
  else
    if Tmap.mem str sE then error ("already defined structuture of name :"^str) p
    else
      let rec aux n m a = function
        |[] -> (m, a)
        |(Param (p1,i,p2,t))::tl -> begin
          if Tmap.mem i a then error ("already existing param name : "^str) p1;
          if exists t sE
          then aux (n+1) (Tmap.add i (n,t) m) (Tmap.add i (b,t,str) a) tl
          else error ("undefined type "^typeName t) p2
          end
      in let rec tList = function
        |[] -> []
        |Param (_,_,_,t)::tl -> t::tList tl
      in let (ajout, aE2) = aux 0 Tmap.empty aE l in
      let typeList = tList l in
      let fE2 =
        if Tmap.mem str fE
        then let liste = Tmap.find str fE in
            Tmap.add str ((List.length liste, typeList, S str)::liste) fE
        else Tmap.add str [(0, typeList, S str)] fE
      in (Tmap.add str (ajout, Tmap.cardinal sE) sE, aE2, fE2)

(* teste la correction d'une déclaration de fonction et la rajoute à l'environnement des fonctions *)
let parcoursFonction (vE:varEnv) (fE:funcEnv) (sE:structEnv) (posStr, nameFunc, pL, posT, pjT, _):funcEnv =
  if nameFunc = "print" || nameFunc = "println" || nameFunc = "div"
  then error ("reserved name "^nameFunc) posStr
  else
    if Tmap.mem nameFunc vE
    then error ("the name \""^nameFunc^"\" is already given to a variable, it can't be a function") posStr
    else
      let (tL,tS) = List.fold_right
        (fun (Param (p1, str, p2, t)) (l,tSet) ->
          if exists t sE
            then if Tset.mem str tSet
              then error ("already defined argument name : "^str^" in function "^nameFunc) p1
              else (t::l,Tset.add str tSet)
            else error ("undefined type : "^typeName t^" in function "^nameFunc) p2)
        pL
        ([],Tset.empty)
      in
      if Tset.cardinal tS <> List.length pL
      then assert false
      else
        if exists pjT sE then
          if Tmap.mem nameFunc fE then
            let l1 = Tmap.find nameFunc fE in
            let rec estDedans a = function
              |[] -> false
              |(_, hd, _)::tl -> (hd=a)||estDedans a tl
            in if estDedans tL l1 then error ("already exiting function "^nameFunc) posStr
            else Tmap.add nameFunc ((List.length l1, tL, pjT)::l1) fE
          else Tmap.add nameFunc [0, tL, pjT] fE
        else error ("undefined type : "^typeName pjT^" in function "^nameFunc) posT


(* Calcule de toutes les variables definies dans les différentes structures *)
let rec chercheDefE (isLoc:bool) (vS:Tset.t) = function
  | Eentier _ | Eflottant _ | Echar _ | Echaine _ | Etrue | Efalse | EentierIdent _ -> vS
  | EentierParG (_, _, (_, eL)) | Ebloc1 (_, eL) -> chercheDefB isLoc vS eL
  | EparDIdent ((_, e), _, _) -> chercheDefE isLoc vS e
  | Eapplication (_, _, eL) -> List.fold_left (fun env (_,e) -> chercheDefE isLoc env e) vS eL
  | Enot (_, e) | Eminus (_, e) -> chercheDefE isLoc vS e
  | Ebinop (_, _, (_, e1), (_, e2)) -> chercheDefE isLoc (chercheDefE isLoc vS e1) e2
  | Elvalue lv -> begin
    match lv with
      | Lident _ -> vS
      | Lindex ((_, e), _, _) -> chercheDefE isLoc vS e
      | Larray ((_, e1), (_, e2)) -> chercheDefE isLoc (chercheDefE isLoc vS e1) e2
    end
  | ElvalueAffect (_, Lident (_, str), (_, e)) -> chercheDefE isLoc (Tset.add str vS) e
  | ElvalueAffect (_, Lindex ((_, e1), _, _), (_, e2)) ->
    chercheDefE isLoc (chercheDefE isLoc vS e1) e2
  | ElvalueAffect (_, Larray ((_, e1), (_, e2)), (_, e3)) ->
    chercheDefE isLoc (chercheDefE isLoc (chercheDefE isLoc vS e1) e2) e3
  | Ereturn (_, None) -> vS
  | Ereturn (_, Some (_, e)) -> chercheDefE isLoc vS e
  | EAssert (_, _, (_, e)) -> chercheDefE isLoc vS e
  | Efor (_, (_, e1), (_, e2), (_, eL)) ->
      let env1 = chercheDefE isLoc (chercheDefE isLoc vS e1) e2 in env1
  | Ewhile ((_, e), (_, eL)) ->
      let env1 = chercheDefE isLoc vS e in env1
  | EdoWhile ((_, eL)) -> vS
  | Eif ((_, e), (_, eL), els) ->
      let env1 = chercheDefE isLoc vS e in
      let env2 = chercheDefB isLoc env1 eL in
      chercheDefElse isLoc env2 els
and chercheDefB (isLoc:bool) (vS:Tset.t) = function
  |[] -> vS
  |(_, e)::tl -> chercheDefB isLoc (chercheDefE isLoc vS e) tl
and chercheDefElse (isLoc:bool) (vS:Tset.t) = function
  |Iend -> vS
  |Ielse (_, eL) -> chercheDefB isLoc vS eL
  |Ielseif ((_, e), (_, eL), els) ->
      let env1 = chercheDefE isLoc vS e in
      let env2 = chercheDefB isLoc env1 eL in
      chercheDefElse isLoc env2 els

(* parcours récursivement l'expression pour tester la définition des différentes variable et les définir (isLoc = is local) *)
let rec parcoursExpr (isLoc:bool) (vE:varEnv) (fE:funcEnv) (aE:argsEnv) (sE:structEnv):Ast.expr -> varEnv = function
  | Eentier _ | Eflottant _ | Echaine _ | Echar _ | Etrue | Efalse -> vE
  | EentierIdent (p, _, ident) -> if Tmap.mem ident vE then vE else error ("undefined variable name "^ident) p
  | EentierParG (_, _, (_, eL)) | Ebloc1 (_, eL) -> parcoursBloc isLoc vE fE aE sE eL
  | EparDIdent ((_, e), p, str) ->
    if Tmap.mem str vE then parcoursExpr isLoc vE fE aE sE e
  else error ("undefined variable name "^str) p
  | Eapplication (pStr, str, eL) ->
      if Tmap.mem str sE || Tmap.mem str fE || str = "print" || str = "println" || str = "_getelement" || str = "_setelement" || str = "newarray"
      then List.fold_left (fun ve (p, e) -> parcoursExpr isLoc ve fE aE sE e) vE eL
      else error ("undefined function 1 "^str) pStr
  | Enot (_, e) | Eminus (_, e) | EAssert (_, _, (_, e)) -> parcoursExpr isLoc vE fE aE sE e
  | Ebinop (_, _, (_, e1), (_, e2)) -> parcoursExpr isLoc (parcoursExpr isLoc vE fE aE sE e1) fE aE sE e2
  | Elvalue lv -> begin
    match lv with
      | Lident (p, str) -> if Tmap.mem str vE then vE else error ("undefined variable name "^str) p
      | Lindex ((_, e), p, str) ->
        let env1 = parcoursExpr isLoc vE fE aE sE e in
        if Tmap.mem str aE then env1 else error ("undefined attribute name "^str) p
      | Larray ((_, e1), (_, e2)) ->
        parcoursExpr isLoc (parcoursExpr isLoc vE fE aE sE e1) fE aE sE e2
    end
  | ElvalueAffect (_, Lident (_, str), (_, e)) ->
    if Tmap.mem str vE then
      if isLoc && not (fst (Tmap.find str vE))
      then (Tmap.add str (isLoc,Any) vE)
    else vE
    else parcoursExpr isLoc (Tmap.add str (isLoc,Any) vE) fE aE sE e
  | ElvalueAffect (_, Lindex ((_, e1), p, str), (_, e2)) ->
    if argExists str aE then
        let env1 = parcoursExpr isLoc vE fE aE sE e1 in
        parcoursExpr isLoc env1 fE aE sE e2
    else error ("undefined attribute name "^str) p
  | ElvalueAffect (_, Larray ((_, e1), (_, e2)), (_, e3)) -> 
      let env1 = parcoursExpr isLoc vE fE aE sE e1 in
      let env2 = parcoursExpr isLoc env1 fE aE sE e2 in
      parcoursExpr isLoc env2 fE aE sE e3
  | Ereturn (p,None) -> vE
  | Ereturn (p, Some (_, e)) -> parcoursExpr isLoc vE fE aE sE e
  | Efor (str, (_, e1), (_, e2), (_, eL)) ->
      let env1 = parcoursExpr isLoc vE fE aE sE e1 in
      let env2 = parcoursExpr isLoc env1 fE aE sE e2 in
      let env3 = Tmap.add str (true,Int64) env2 in
      let env4 = let newdef = chercheDefB isLoc Tset.empty eL in
          Tset.fold (fun x m -> if Tmap.mem x m then m else Tmap.add x (true, Any) m)  newdef (Tmap.filter (fun k (b,t) -> b || not (Tset.mem k newdef)) env3)
      in let _ = parcoursBloc true env4 fE aE sE eL
      in env2
  | Ewhile ((_, e), (_, eL)) ->
      let env1 = parcoursExpr isLoc vE fE aE sE e in
      let env2 = let newdef = chercheDefB isLoc Tset.empty eL in
          Tset.fold (fun x m -> if Tmap.mem x m then m else Tmap.add x (true, Any) m)  newdef (Tmap.filter (fun k (b,t) -> b || not (Tset.mem k newdef)) env1)
      in let _ = parcoursBloc true env2 fE aE sE eL
      in env1
  | EdoWhile ((_, eL)) -> vE
  | Eif ((_, e), (_, eL), els) ->
      let env1 = parcoursExpr isLoc vE fE aE sE e in
      let env2 = parcoursBloc isLoc env1 fE aE sE eL in
      parcoursElse isLoc env2 fE aE sE els
and parcoursBloc (isLoc:bool) (vE:varEnv) (fE:funcEnv) (aE:argsEnv) (sE:structEnv) = function
  |[] -> vE
  |(_, e)::tl -> parcoursBloc isLoc (parcoursExpr isLoc vE fE aE sE e) fE aE sE tl
and parcoursElse (isLoc:bool) (vE:varEnv) (fE:funcEnv) (aE:argsEnv) (sE:structEnv) = function
  |Iend -> vE
  |Ielse (_, eL) -> parcoursBloc isLoc vE fE aE sE eL
  |Ielseif ((_, e), (_, eL), els) ->
      let env1 = parcoursExpr isLoc vE fE aE sE e in
      let env2 = parcoursBloc isLoc env1 fE aE sE eL in
      parcoursElse isLoc env2 fE aE sE els

(* dispatch sur les différentes fonctions présedente les élément du fichier pour le premiers parcours (rT = types possible d'un return et b = les return sont autorisé)*)
let rec parcours1 (vEnv:varEnv) (fEnv:funcEnv) (sEnv:structEnv) (aEnv:argsEnv) = function
  |[] -> (vEnv, fEnv, sEnv, aEnv)
  |Dstruct (b, p, i, pL)::tl -> begin
      let s,a,f = parcoursStruct sEnv aEnv fEnv (b, p, i, pL) in
      parcours1 vEnv f s a tl
      end
  |Dfonction (p1, i, pL, p2, pT, b, _)::tl -> begin
      let fEnv2 = parcoursFonction vEnv fEnv sEnv (p1, i, pL, p2, pT, b) in
      parcours1 vEnv fEnv2 sEnv aEnv tl
      end
  |Dexpr (_, e)::tl -> begin
      let vEnv2 = parcoursExpr (not !estCompile) vEnv fEnv aEnv sEnv e in
      parcours1 vEnv2 fEnv sEnv aEnv tl
      end

(* teste le typage d'une expression *)
let rec testTypageE (isLoc:bool) (vE:varEnv) (fE:funcEnv) (sE:structEnv) (aE:argsEnv) (rT:Astype.pjtype) (b:bool):Ast.expr -> expressionTyper = function
  | Eentier i -> Int64, EntierE i
  | Eflottant f -> Float64, FlottantE f
  | Echaine str -> Array, ChaineE str
  | Echar c -> Char64, CharE c
  | Etrue -> Bool, TrueE
  | Efalse -> Bool, FalseE
  | EAssert (l, n, (p, e)) -> 
    let (t,et) = (testTypageE isLoc vE fE sE aE rT b e) in
    if compatible t Bool then Nothing, AssertE (l, n, (t, et))
    else error ("found "^typeName t^" which is not compatible with Bool") p
  | EentierIdent (p, i, str) ->
      let var =
        try snd (Tmap.find str vE)
        with _ -> error ("undefined variable name " ^ str) p
      in
      if compatible Int64 var
      then Int64, BinopE (Times, (Int64, EntierE i), (var, LvalueE (IdentL (var, str, fst (Tmap.find str vE)))))
      else error ("not compatible Int64 with "^typeName (snd (try Tmap.find str vE with Not_found -> assert false))) p
  | EentierParG (_, i, (pb, eL)) ->
      let t,eLt = (testTypEBloc isLoc vE fE sE aE rT b eL) in
      if compatible Int64 t
      then Int64, BinopE (Times, (Int64, EntierE i), (t, BlocE (t, eLt)))
      else error ("not compatible Int64 with "^typeName t) pb
  | Ebloc1 (_,eL) ->
    let (t, eLt) = testTypEBloc isLoc vE fE sE aE rT b eL in
    t, BlocE (t, eLt)
  | EparDIdent ((pE, e), pI, ident) -> begin
    let variable = try snd (Tmap.find ident vE) with _ -> error ("undefined variable name " ^ ident) pI in
    let (t,et) = (testTypageE isLoc vE fE sE aE rT b e) in
    match variable, t with
      |Int64, Int64 -> Int64, BinopE (Times, (t, et), (variable, LvalueE (IdentL (variable, ident, fst (Tmap.find ident vE)))))
      |Float64, Float64 |Any, Float64 |Float64, Any -> Float64, BinopE (Times, (t, et), (variable, LvalueE (IdentL (variable, ident, fst (Tmap.find ident vE)))))
      |Float64, Int64 | Int64, Float64 -> Float64, BinopE (Times, (t, et), (variable, LvalueE (IdentL (variable, ident, fst (Tmap.find ident vE)))))
      |Any, Any | Int64, Any | Any, Int64 -> Any, BinopE (Times, (t, et), (variable, LvalueE (IdentL (variable, ident, fst (Tmap.find ident vE)))))
      |Int64, _ | Float64, _ | Any, _ -> error ("found "^typeName t^" which is not compatible for mult") pE
      |_, Int64 | _, Float64 | _, Any -> error ("found "^typeName variable^" which is not compatible for mult") pI
      |_,_ -> error (Logo.booom) pI
    end
  | Eapplication (pName, ident, eL) -> begin
      match ident with 
      | "print" | "println" | "delay" -> (Nothing, CallE ((ident, ISet.singleton 0), List.fold_right (fun (_, e) l -> testTypageE isLoc vE fE sE aE rT b e::l) eL []))
      | "newarray" | "_getelement" | "_setelement" -> (Any, CallE ((ident, ISet.singleton 0), List.fold_right (fun (_, e) l -> testTypageE isLoc vE fE sE aE rT b e::l) eL []))
      | _ ->
      if Tmap.mem ident fE then
        let l = try Tmap.find ident fE with Not_found -> assert false in
        let rec calcTyp l = match l with
          |[] -> []
          |(_, e)::tl -> (testTypageE isLoc vE fE sE aE rT b e)::calcTyp tl
        in let argL = calcTyp eL in
        let argTL = List.map (fun (t, _) -> t) argL in
        let rec aux l1 l2 = match l1,l2 with
          |[],[] -> ([],0,true)
          |t::tl1, t2::tl2 -> begin
            let (l,nb, b) = aux tl1 tl2 in
            if b && compatible t t2
            then if t2=Any then (t::l, nb+(if t=Any then 1 else 0), true) else (l, nb+(if t=Any then 1 else 0), true)
            else ([], 0, false)
            end
          | _, _ -> ([], 0, false)
        in
        let (tSet, fL) =
          List.fold_left (fun (s, fL)  (i, pL, pjT) ->
                    let (tRestant, n, b) = aux pL argTL in
                    if b then
                      (TypeSet.add pjT s, rajouteFonction (i, n, tRestant) fL)
                    else (s, fL)
                  )
                  (TypeSet.empty, [])
                  l in
        begin match fL with
          |[] -> error ("no compatible function "^ident) pName
          |[iSet, _, tR] -> if ISet.cardinal iSet > 1
            then error ("ambiguity in function call "^ident) pName
            else (if TypeSet.cardinal tSet = 1 then TypeSet.choose tSet else Any), CallE ((ident, iSet), argL)
          |l -> (if TypeSet.cardinal tSet = 1 then TypeSet.choose tSet else Any), CallE ((ident, List.fold_left (fun iS (i, _, _) -> ISet.union iS i) ISet.empty l), argL)
        end
       else error ("undeclared function "^ident) pName
    end
  | Enot (p, e) ->
    let (t, et) = testTypageE isLoc vE fE sE aE rT b e in
    if compatible Bool t
    then Bool,NotE (t, et)
    else error ("incompatibility of type in Not "^typeName t) p
  | Eminus (p, e) -> begin
    let (t, et) = testTypageE isLoc vE fE sE aE rT b e in
    match t with
      |Int64 -> Int64, MinusE (t, et)
      |Float64 -> Float64, MinusE (t, et)
      |Any -> Any, MinusE (t, et)
      | _ -> error ("uncompatible type expected Int64/Float64 but got an "^typeName t) p
    end
  | Ebinop (p, o, (p1, e1), (p2, e2)) -> begin
    let (t1, et1) = testTypageE isLoc vE fE sE aE rT b e1 in
    let (t2, et2) = testTypageE isLoc vE fE sE aE rT b e2 in
    match o with
      |Eq | Neq -> Bool, BinopE (o, (t1, et1), (t2, et2))
      |Lo | Gr | Leq | Geq ->
        if (compatible t1 Bool || compatible t1 Int64 || compatible t1 Float64)
            && (compatible t2 Bool || compatible t2 Int64 || compatible t2 Float64)
        then Bool,BinopE (o, (t1, et1), (t2, et2)) else error ("not compatible in comparison : "^typeName t1^"!="^typeName t2) p
      |And | Or ->
        if compatible t1 Bool
        then if compatible t2 Bool
          then Bool, BinopE (o, (t1, et1), (t2, et2))
          else error ("expected a Bool but got a "^typeName t2) p2
        else error ("expected a Bool but got a "^typeName t1) p1
      | Plus | Minus | Times | Modulo | Exp -> begin
        match t1,t2 with
        |Int64, Int64 -> Int64, BinopE (o, (t1, et1), (t2, et2))
        |Float64, Float64 |Any, Float64 |Float64, Any -> Float64, BinopE (o, (t1, et1), (t2, et2))
        |Float64, Int64 | Int64, Float64 -> Float64, BinopE (o, (t1, et1), (t2, et2))
        |Any, Any |Any, Int64 |Int64, Any -> Any, BinopE (o, (t1, et1), (t2, et2))
        |_, _ -> error ("found "^typeName t1^" and "^typeName t2^" which aren't good for a bin-op") p
        end
      | Concat -> begin match t1,t2 with
        |_,_ -> Array, BinopE (o, (t1, et1), (t2, et2))
        end
      | Bit_Or | Bit_And | Bit_Xor -> t1, BinopE (o, (t1, et1), (t2, et2)) (* To be ameliored when time is available *)
      | Shift_Left | Shift_Right -> 
        begin
        match t2 with 
        | Int64 | Any -> t1, BinopE (o, (t1, et1), (t2, et2))
        | _ -> error ("found " ^ typeName t2 ^ " and is not a good type for shift amount") p
        end
    end
  | Elvalue lv -> begin
    match lv with
      |Lident (p,str) -> let t = snd (try Tmap.find str vE with Not_found -> print_int p.ldeb; print_newline ();assert false) in t, LvalueE (IdentL (t, str, fst (Tmap.find str vE)))
      |Lindex ((_, e), p, n) ->
        let (b, t2, nm) = try Tmap.find n aE with Not_found -> assert false in
        let (t3, et3) = testTypageE isLoc vE fE sE aE rT b e in
        if compatible t3 (S nm) then t2, LvalueE (IndexL ((t3, et3), nm, n))
        else error ("type incompatibility in index "^typeName t3^" not compatible with struct "^nm) p
      | Larray ((p1, e1), (p2, e2)) ->
        let t1, e1 = testTypageE isLoc vE fE sE aE rT b e1 in
        let t2, e2 = testTypageE isLoc vE fE sE aE rT b e2 in
        if compatible t1 Array then
          if compatible t2 Int64 then
            Any, LvalueE (ArrayL ((t1, e1), (t2, e2)))
          else error ("type incompatibility "^typeName t2^" not compatible with int") p2
        else error ("type incompatibility "^typeName t1^" not compatible with array") p1

    end
  | ElvalueAffect (pEqual, lv, (pe, e)) -> begin
    let (t, et) = testTypageE isLoc vE fE sE aE rT b e in
    match lv with
      | Lident (p, str) ->
        if Tmap.mem str fE then error (str^" is also a function, can't be both") p
        else
          let t2 = snd (try Tmap.find str vE with Not_found -> assert false) in
          if compatible t t2 then (if t = Any then t2 else t),LvalueAffectE (IdentL (t2, str, fst (Tmap.find str vE)), (t, et))
          else error ("type incompatibility in affectation : "^typeName t^" can't be given to "^str^" who has type "^typeName t2) pEqual
      | Lindex ((pe2, e2), pDot, n) ->
        let (_mutable, t2, nm) = try Tmap.find n aE with Not_found -> assert false in
        if _mutable then
          let (t3, et3) = testTypageE isLoc vE fE sE aE rT b e2 in
          if compatible t3 (S nm)
          then if compatible t t2
            then (if t = Any then t2 else t), LvalueAffectE (IndexL ((t3, et3), nm, n), (t, et))
            else error ("type incompatibility in index affectation : "^typeName t^" can't be given to var who has type "^typeName t2) pEqual
          else error ("type incompatibility in index affectation : "^typeName t3^" is not compatible with Struct "^nm) pDot
        else error ("type incompatibility in index affectation : "^nm^" who has "^n^" as an attribute is not mutable") pDot
      | Larray ((p1, e1), (p2, e2)) ->
        let t1, e1 = testTypageE isLoc vE fE sE aE rT b e1 in
        let t2, e2 = testTypageE isLoc vE fE sE aE rT b e2 in
        if compatible t2 Int64 then
          if compatible t1 Array then
            t, LvalueAffectE (ArrayL ((t1, e1), (t2, e2)), (t, et))
          else error ("type incompatibility "^typeName t1^" not compatible with array") p1
        else error ("type incompatibility "^typeName t2^" not compatible with int") p2

    end
  | Ereturn (p, opt) -> if b
    then begin
      match opt with
        | None -> if compatible rT Nothing then rT, ReturnE (Nothing, None) else error ("Expected a "^typeName rT^" but found a Nothing") p
        | Some (pe, e) -> let (t, et) = testTypageE isLoc vE fE sE aE rT b e in
          if compatible rT t then Any, ReturnE (rT, Some (t, et))
          else  error ("Expected a "^typeName rT^" but found a "^typeName t) pe
      end
    else error "Returns must appear inside functions" p
  | Efor (i, (p1, e1), (p2, e2), (pb, eL)) ->
    let vE1 = parcoursExpr isLoc vE fE aE sE e1 in
    let vE2 = parcoursExpr isLoc vE1 fE aE sE e2 in
    let vE3 = let newdef = chercheDefB isLoc Tset.empty eL in
          Tset.fold (fun x m -> if Tmap.mem x m then m else Tmap.add x (true, Any) m)  newdef (Tmap.filter (fun k (b,t) -> b || not (Tset.mem k newdef)) vE2) in
    let vE3 = Tmap.add i (true, Int64) vE3 in
    let v = Tmap.map (fun (_,t) -> t) (Tmap.filter (fun k (b,t) -> if Tmap.mem k vE then b != fst (try Tmap.find k vE with Not_found -> assert false) else true) vE3) in
    let (t1, et1) = testTypageE isLoc vE1 fE sE aE rT b e1 in
    if compatible t1 Int64
    then let (t2, et2) = testTypageE isLoc vE2 fE sE aE rT b e2 in
      if compatible t2 Int64
      then let blocFor = testTypEBloc true vE3 fE sE aE rT b eL in
        Nothing, ForE (i, v, (t1, et1), (t2, et2), blocFor)
      else error ("expected an Int64 but got an "^typeName t2) p2
    else error ("expected an Int64 but got an "^typeName t1) p1
  | Ewhile ((pe, e), (pb, eL)) ->
    let vE2 = let newdef = chercheDefB true Tset.empty eL in
          Tset.fold (fun x m -> if Tmap.mem x m then m else Tmap.add x (true, Any) m) newdef (Tmap.filter (fun k (b,t) -> b || not (Tset.mem k newdef)) vE) in
    let (t, et) = testTypageE isLoc vE2 fE sE aE rT b e in
    let v = Tmap.map (fun (_,t) -> t) (Tmap.filter (fun k (b,t) -> if Tmap.mem k vE then b != fst (try Tmap.find k vE with Not_found -> assert false) else true) vE2) in
    if compatible Bool t
    then let blocWhile = testTypEBloc true vE2 fE sE aE rT b eL in
      Nothing, WhileE ((t, et), v, blocWhile)
    else error ("expected a Bool but got an "^typeName t) pe
  | EdoWhile ((pb, eL)) ->
    let vE2 = parcoursBloc true vE fE aE sE eL in
    let v = Tmap.map (fun (_,t) -> t) (Tmap.filter (fun k (b,t) -> if Tmap.mem k vE then b != fst (try Tmap.find k vE with Not_found -> assert false) else true) vE2) in
    let blocWhile = testTypEBloc true vE2 fE sE aE rT b eL in
    if compatible Bool (fst blocWhile)
    then
      Nothing, DoWhileE (v, blocWhile)
    else error ("expected a Bool but got an "^typeName (fst blocWhile)) pb
  | Eif ((pe, e), (pb, eL), els) ->
      let (t, et) = testTypageE isLoc vE fE sE aE rT b e in
      if compatible Bool t
      then let (tb, bt) = testTypEBloc isLoc vE fE sE aE rT b eL in
        let(tE, elt) = testTypEElse isLoc vE fE sE aE rT b els in
        (if tb = tE then tE else Any), IfE ((t, et), (tb, bt), elt)
      else error ("expected a Bool but got an "^typeName t) pe
and testTypEBloc (isLoc:bool) (vE:varEnv) (fE:funcEnv) (sE:structEnv) (aE:argsEnv) (rT:Astype.pjtype) (b:bool):expression list -> blocTyper = function
  |[] -> Nothing,[]
  |[(p,e)] -> let (t, et) = testTypageE isLoc vE fE sE aE rT b e in t, [t,et]
  |(p,e)::tl ->
      let e2 = testTypageE isLoc vE fE sE aE rT b e in
      let (tb, bt) = testTypEBloc isLoc vE fE sE aE rT b tl in (tb, e2::bt)
and testTypEElse (isLoc:bool) (vE:varEnv) (fE:funcEnv) (sE:structEnv) (aE:argsEnv) (rT:Astype.pjtype) (b:bool) = function
  |Iend -> Nothing, EndI
  |Ielse (pb, eL) -> let (t, bt) = testTypEBloc isLoc vE fE sE aE rT b eL in t, ElseI (t, bt)
  |Ielseif ((pe, e), (pb, eL), els) ->
    let (te, et) = testTypageE isLoc vE fE sE aE rT b e in
    if compatible te Bool then
      let (tb, bt) = testTypEBloc isLoc vE fE sE aE rT b eL in
      let (t, elt) = testTypEElse isLoc vE fE sE aE rT b els in
      (if t=tb then t else Any), ElseifI ((te, et), (tb, bt), elt)
    else error ("expected a Bool but got a "^typeName te) pe

(* calcule la dernière expression d'un bloc (utile pour le typage des fonctions) *)
let rec lastInstruction v = function
  |[] -> v
  |v2::tl -> lastInstruction v2 tl

(* teste le typage d'une fonction *)
let testTypageF (vE:varEnv) (fE:funcEnv) (sE:structEnv) (aE:argsEnv) (posN, str, pL, posT, pjT, (pb, eL)) (fonctions:funcMap) =
  let listP = List.fold_right (fun (Param (_, str, _, t)) l -> (str, t)::l) pL [] in
  let newdef = chercheDefB true Tset.empty eL in
  let vE0 = Tset.fold (fun x m -> if Tmap.mem x m then m else Tmap.add x (true, Any) m)  newdef (Tmap.filter (fun k t ->not (Tset.mem k newdef)) vE) in
  let vE1 = List.fold_left (fun m (str, t) -> Tmap.add str (true, t) m) vE0 listP in
  let vE2 = try parcoursBloc true vE1 fE aE sE eL with Not_found -> assert false in
  let vE3 = List.fold_left (fun m (str, t) -> Tmap.add str (true, t) m) vE2 listP in
  let varSet = Tmap.map (fun (_, t) -> t) (Tmap.filter (fun k (b,t) -> if Tmap.mem k vE then b != fst (try Tmap.find k vE with Not_found -> assert false) else true) vE3) in
  let (tb, bt) = try testTypEBloc true vE3 fE sE aE pjT true eL with Not_found -> (print_string "Not_found in function bloc\n"; raise Not_found) in
  if compatible pjT tb
  then if Tmap.mem str fonctions
    then
      let imap = try Tmap.find str fonctions with Not_found -> assert false in
      let imap2 = Imap.add (Imap.cardinal imap) (Funct (listP, varSet, (tb, bt), pjT)) imap in
      Tmap.add str imap2 fonctions
    else Tmap.add str (Imap.singleton 0 (Funct (listP, varSet, (tb, bt), pjT))) fonctions
  else error ("last intruction not compatible "^typeName tb^"-"^typeName pjT) pb

(* effectue le deuxième parcours *)
let rec parcours2 (vEnv:varEnv) (fEnv:funcEnv) (sEnv:structEnv) (aEnv:argsEnv) (fonctions:funcMap)= function
  |[] ->  (vEnv, fEnv, sEnv, aEnv, [], fonctions)
  |Dstruct (b, position, ident, pL)::tl ->
    let fonctions2 =
      let pL = List.fold_right (fun (Param (_, str, _, t)) l -> (str, t)::l) pL [] in
      if Tmap.mem ident fonctions then
        let imap = Tmap.find ident fonctions in
        let imap2 = Imap.add (Imap.cardinal imap) (StructBuilder pL) imap in
        Tmap.add ident imap2 fonctions
      else Tmap.add ident (Imap.singleton 0 (StructBuilder pL)) fonctions
    in parcours2 vEnv fEnv sEnv aEnv fonctions2 tl
  |Dfonction  (a, str, b, c, d, e, _)::tl ->
    let fonctions2 = try testTypageF vEnv fEnv sEnv aEnv (a, str, b, c, d, e) fonctions with Not_found -> failwith "not found in func"
    in parcours2 vEnv fEnv sEnv aEnv fonctions2 tl
  |Dexpr (_, e)::tl ->
    let (t, et) = try testTypageE false vEnv fEnv sEnv aEnv Any false e with Not_found -> failwith "not found in expr" in
    let (a, b, c, d, eL, f) = parcours2 vEnv fEnv sEnv aEnv fonctions tl in
    (a, b, c, d, et::eL, f)

(* fonction globale de vérification du type fait d'utiliser des réfenrence est nécessaire au bon fonctionnement du REPL *)
let verificationType (declL:Ast.fichier) (envV:varEnv ref) (envF:funcEnv ref) (envS:structEnv ref) (envA:argsEnv ref):fichierTyper =
    let DeclarationList dl = declL in
    let vE, fE,sE,aE = parcours1 !envV !envF !envS !envA dl in
    let vp, fp, sp, ap, eL, fonctions = parcours2 (Tmap.add "nothing" (false, Nothing) vE) fE sE aE Tmap.empty dl in
    envV := vp;
    envF := fp;
    envS := sp;
    envA := ap;
    (eL, Tmap.map (fun (_, t) -> t) vp, sp, fonctions, fp)

let resetVE (v:varEnv ref) =
  v := Tmap.singleton "nothing" (false, Nothing)

let resetFE (v:funcEnv ref) =
  v := Tmap.singleton "div" [(0, [Int64; Int64], Int64); (1, [Float64; Int64], Float64); (2, [Int64; Float64], Float64); (3, [Float64; Float64], Float64)];
  v := Tmap.add "int" [(0, [Float64], Int64); (1, [Int64], Int64); (2, [Char64], Int64); (3, [Bool], Int64)] !v;
  v := Tmap.add "float" [(0, [Int64], Float64); (1, [Float64], Float64)] !v;
  v := Tmap.add "input_int" [(0, [], Int64)] !v;
  v := Tmap.add "array_length" [(0, [Array], Int64)] !v;
  v := Tmap.add "sqrt" [(0, [Int64], Float64); (1, [Float64], Int64)] !v;
  v := Tmap.add "delay" [(0, [Int64], Nothing)] !v;
  v := Tmap.add "timestamp" [(0, [], Int64)] !v;
  v := Tmap.add "char" [(0, [Int64], Char64)] !v;
  v := Tmap.add "typeof" [(0, [Any], Int64)] !v

let resetSE (v:structEnv ref) = 
  v := Tmap.empty

let resetAE (v:argsEnv ref) =
  v := Tmap.empty 


(* fonctions d'interface avec l'extérieur *)
let typerCompilateur = (fun a -> estCompile := true; verificationType a)
let typerRepl = (fun a -> estCompile := false; verificationType a)
