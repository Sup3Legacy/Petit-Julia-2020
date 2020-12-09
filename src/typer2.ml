open Astype2
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

(* Teste si f est identiques à toutes les autres fonctions dans la liste *)
let rec compatibleFInL ((n1,l1) as f) = function
  |[] -> true
  |(n2,l2)::tl -> n1==n2 && l1==l2 && compatibleFInL f tl

(* lanceur d'erreur *)
let error (msg:string) (p:Ast.position) = raise (Ast.Typing_Error_Msg_Pos (msg,p) )

(* teste si le type t existe bien *)
let exists (t:Astype.pjtype) (env:structEnv):bool = match t with
  | Any | Nothing | Int64 | Float64 | Bool | String -> true
  | S s -> Tmap.mem s env

(* teste si le champ existe *)
let argExists (t:string) (env:argsEnv) = Tmap.mem t env

(* convertie le type en une string pour les messages d'erreur *)
let typeName (t:Astype.pjtype):string = match t with
  | Int64 -> "Int64"
  | Float64 -> "Float64"
  | Nothing -> "Nothing"
  | Bool -> "Bool"
  | String -> "String"
  | S s -> "Struct \""^s^"\""
  | Any -> "Any"

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
      in let (ajout,aE2, lChamps) = aux 0 Tmap.empty aE l in
      let typeList = tList l in
      let fE2,fMap2 =
        if Tmap.mem str fE
        then let liste = Tmap.find str fE in
            Tmap.add str ((List.length liste, typeList, S str)::liste) fE
        else Tmap.add str [(0, typeList, S str)] fE
      in (Tmap.add str ajout sE, aE2, fE2, fMap2)

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
              |(hd,_)::tl -> (hd=a)||estDedans a tl
            in if estDedans tL l1 then error ("already exiting function "^nameFunc) posStr
            else Tmap.add nameFunc ((List.length l1, tL, pjT)::l1) fE
          else Tmap.add nameFunc [0, tL, pjT] fE
        else error ("undefined type : "^typeName pjT^" in function "^nameFunc) posT


(* Calcule de toutes les variables definies dans les différentes structures *)
let rec chercheDefE (isLoc:bool) (vS:Tset.t) = function
  | Eentier _ | Eflottant _ | Echaine _ | Etrue | Efalse | EentierIdent _ -> vS
  | EentierParG (_, _, (_, eL)) | Ebloc1 (_, eL) -> chercheDefB isLoc vS eL
  | EparDIdent ((_, e), _, _) -> chercheDefE isLoc vS e
  | Eapplication (_, _, eL) -> List.fold_left (fun env (_,e) -> chercheDefE isLoc env e) vS eL
  | Enot (_, e) | Eminus (_, e) -> chercheDefE isLoc vS e
  | Ebinop (_, _, (_, e1), (_, e2)) -> chercheDefE isLoc (chercheDefE isLoc vS e1) e2
  | Elvalue lv -> begin
    match lv with
      |Lident _ -> vS
      |Lindex ((_, e), _, _) -> chercheDefE isLoc vS e
    end
  | ElvalueAffect (_, Lident (_, str), (_, e)) -> chercheDefE isLoc (Tset.add str vS) e
  | ElvalueAffect (_, Lindex ((_, e1), _, _), (_, e2)) ->
    chercheDefE isLoc (chercheDefE isLoc vS e1) e2
  | Ereturn (_, None) -> vS
  | Ereturn (_, Some (_, e)) -> chercheDefE isLoc vS e
  | Efor (_, (_, e1), (_, e2), (_, eL)) ->
      let env1 = chercheDefE isLoc (chercheDefE isLoc vS e1) e2 in
      if false && isLoc then chercheDefB isLoc env1 eL else env1
  | Ewhile ((_, e), (_, eL)) ->
      let env1 = chercheDefE isLoc vS e in
      if false && isLoc then chercheDefB isLoc env1 eL else env1
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
  | Eentier _ | Eflottant _ | Echaine _ | Etrue | Efalse -> vE
  | EentierIdent (p, _, ident) -> if Tmap.mem ident vE then vE else error ("undefined variable name "^str) p
  | EentierParG (_, _, (_, eL)) | Ebloc1 (_, eL) -> parcoursBloc isLoc vE fE aE sE eL
  | EparDIdent ((_, e), p, str) ->
    if Tmap.mem str vE then parcoursExpr isLoc vE fE aE sE e
  else error ("undefined variable name "^str) p
  | Eapplication (pStr, str, eL) ->
      if Tmap.mem str sE || Tmap.mem str fE || str = "print" || str = "println"
      then List.fold_left (fun ve (p, e) -> parcoursExpr isLoc ve fE aE sE e) vE eL
      else error ("undefined function 1 "^str) pStr
  | Enot (_, e) | Eminus (_, e) -> parcoursExpr isLoc vE fE aE sE e
  | Ebinop (_, _, (_, e1), (_, e2)) -> parcoursExpr isLoc (parcoursExpr isLoc vE fE aE sE e1) fE aE sE e2
  | Elvalue lv -> begin
    match lv with
      |Lident (p, str) -> if Tmap.mem str vE then vE else error ("undefined variable name "^str) p
      |Lindex ((_, e), p, str) -> let env1 = parcoursExpr isLoc vE fE aE sE e in
        if Tmap.mem str aE then env1 else error ("undefined attribute name "^str) p
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
  | Ereturn (p,None) -> vE
  | Ereturn (p, Some (_, e)) -> parcoursExpr isLoc vE fE aE sE e
  | Efor (str, (_, e1), (_, e2), (_, eL)) ->
      let env1 = parcoursExpr isLoc vE fE aE sE e1 in
      let env2 = parcoursExpr isLoc env1 fE aE sE e2 in
      let env3 = Tmap.add str (true,Int64) env2 in
      let env4 = if isLoc then env3
        else (let newdef = chercheDefB isLoc Tset.empty eL in
          Tmap.filter (fun k t ->not (Tset.mem k newdef)) env3)
      in let _ = parcoursBloc true env4 fE aE sE eL
      in env2
  | Ewhile ((_, e), (_, eL)) ->
      let env1 = parcoursExpr isLoc vE fE aE sE e in
      let env2 = if isLoc then env1
        else (let newdef = chercheDefB isLoc Tset.empty eL in
          Tmap.filter (fun k t -> not (Tset.mem k newdef)) env1)
      in let _ = parcoursBloc true env2 fE aE sE eL
      in env2
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
  | Echaine str -> String, ChaineE str
  | Etrue -> Bool, TrueE
  | Efalse -> Bool, FalseE
  | EentierIdent (p, i, str) ->
      let var =
        try snd (Tmap.find str vE)
        with _ -> error ("undefined variable name " ^ str) p
      in
      if compatible Int64 var
      then Int64, EntierIdentE (i, var, str)
      else error ("not compatible Int64 with "^typeName (snd (Tmap.find str vE))) p
  | EentierParG (_, i, (pb, eL)) ->
      let t,eLt = (testTypEBloc isLoc vE fE sE aE rT b eL) in
      if compatible Int64 t
      then Int64, EntierParGE (i, (t, eLt))
      else error ("not compatible Int64 with "^typeName t) pb
  | Ebloc1 (_,eL) ->
    let (t, eLt) = testTypEBloc isLoc vE fE sE aE rT b eL in
    t, BlocE (t, eLt)
  | EparDIdent ((pE, e), pI, ident) -> begin 
    let variable = try snd (Tmap.find ident vE) with _ -> error ("undefined variable name " ^ ident) pI in
    let (t,et) = (testTypageE isLoc vE fE sE aE rT b e) in
    match variable, t with
      |Int64, Int64 -> Int64, ParDIdentE ((t, et), variable, ident)
      |Float64, Float64 |Any, Float64 |Float64, Any -> Float64, ParDIdentE ((t, et), variable, ident)
      |Float64, Int64 | Int64, Float64 -> Float64, ParDIdentE ((t, et), variable, ident)
      |Any, Any | Int64, Any | Any, Int64 -> Any, ((t, et), variable, ident)
    end
  | Eapplication (pName, ident, eL) -> begin
    if ident = "print" || ident = "println"
    then
      Nothing, CallE ((ident, ISet.singleton 0), List.fold_right (fun (_, e) l -> testTypageE isLoc vE fE sE aE rT b e::l) eL []) 
    else
      if Tmap.mem ident fE then
        let l = Tmap.find ident fE in
        let rec calcTyp = function
          |[] -> []
          |(_, e)::tl -> testTypageE isLoc vE fE sE aE rT b e::calcTyp tl
        in let argL = calcTyp eL in
        in let argTL = List.map (fun (t,_) -> t) eL in
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
          |l -> (if TypeSet.cardinal tSet = 1 then TypeSet.choose tSet else Any), CallE ((ident, ), argL)
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
      |Eq | Neq -> Bool
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
        end
    end
  | Elvalue lv -> begin
    match lv with
      |Lident (p,str) -> let t = snd (Tmap.find str vE) in t, LvalueE (IdentL (t, str))
      |Lindex ((_, e), p, n) ->
        let (b, t2, nm) = Tmap.find n aE in
        let (t3, et3) = testTypageE isLoc vE fE sE aE rT b e in
        if compatible t3 (S nm) then t2, LvalueE (IndexL ((t3, et3), nm, n))
        else error ("type incompatibility in index "^typeName t3^" not compatible with struct "^nm) p
    end
  | ElvalueAffect (pEqual, lv, (pe, e)) -> begin
    let (t, et) = testTypageE isLoc vE fE sE aE rT b e in
    match lv with
      | Lident (p, str) ->
        if Tmap.mem str fE then error (str^" is also a function, can't be both") p
        else
          let t2 = snd (Tmap.find str vE) in
          if compatible t t2 then (if t = Any then t2 else t),LvalueAffectE (IdentL (t2, str), (t, et))
          else error ("type incompatibility in affectation : "^typeName t^" can't be given to "^str^" who has type "^typeName t2) pEqual
      | Lindex ((pe2, e2), pDot, n) ->
        let (_mutable, t2, nm) = Tmap.find n aE in
        if _mutable then
          let (t3, et3) = testTypageE isLoc vE fE sE aE rT b e2 in
          if compatible t3 (S nm)
          then if compatible t t2
            then (if t = Any then t2 else t), LvalueAffectE (IndexL ((t3, et3), nm, n), (t, et))
            else error ("type incompatibility in index affectation : "^typeName t^" can't be given to var who has type "^typeName t2) pEqual
          else error ("type incompatibility in index affectation : "^typeName t3^" is not compatible with Struct "^nm) pDot
        else error ("type incompatibility in index affectation : "^nm^" who has "^n^" as an attribute is not mutable") pDot
    end
  | Ereturn (p, opt) -> if b
    then begin
      match opt with
        | None -> if compatible rT Nothing then rT, ReturnE (Nothing, None) else error ("Expected a "^typeName rT^" but found a Nothing") p
        | Some (pe, e) -> let (t, et) = testTypageE isLoc vE fE sE aE rT b e in
          if compatible rT t then Any, ReturnE (rT, (t, et))
          else  error ("Expected a "^typeName rT^" but found a "^typeName t) pe
      end
    else error "Returns must appear inside functions" p
  | Efor (i, (p1, e1), (p2, e2), (pb, eL)) ->
    let vE1 = parcoursExpr isLoc vE fE aE sE e1 in
    let vE2 = parcoursExpr isLoc vE1 fE aE sE e2 in
    let vE3 = parcoursBloc true (Tmap.add i (true, Int64) vE2) fE aE sE eL in
    let v = Tmap.map (fun (_,t) -> t) (Tmap.filter (fun k (b,t) -> if Tmap.mem vE then b != fst (Tmap.find k vE) else true) vE3) in
    let (t1, et1) = testTypageE isLoc vE1 fE sE aE rT b e1 in
    if compatible t1 Int64
    then let (t2, et2) = testTypageE isLoc vE2 fE sE aE rT b e2 in
      if compatible t2 Int64
      then let blocFor = testTypEBloc true vE3 fE sE aE rT b eL in
        Nothing, ForE (i, v,(t1, et1), (t2, et2), blocFor)
      else error ("expected an Int64 but got an "^typeName t2) p2
    else error ("expected an Int64 but got an "^typeName t1) p1
  | Ewhile ((pe, e), (pb, eL)) ->
    let vE2 = parcoursBloc true vE fE aE sE eL in
    let (t, et) = testTypageE isLoc vE2 fE sE aE rT b e in
    let v = Tmap.map (fun (_,t) -> t) (Tmap.filter (fun k (b,t) -> if Tmap.mem vE then b != fst (Tmap.find k vE) else true) vE2) in
    if compatible Bool t
    then let blocWhile = testTypEBloc true vE fE sE aE rT b eL in
      Nothing, WhileE ((t, et), v, blocWhile)
    else error ("expected a Bool but got an "^typeName t) pe
  | Eif ((pe, e), (pb, eL), els) ->
      let (t, et) = testTypageE isLoc vE fE sE aE rT b e in
      if compatible Bool t
      then let (tb, bt) = testTypEBloc isLoc vE fE sE aE rT b eL in
        let(tE, elt) = testTypEElse isLoc vE fE sE aE rT b els in
        (if tb = tE then t1 else Any), IfE ((t, et), (tb, bt), elt)
      else error ("expected a Bool but got an "^typeName t) pe
and testTypEBloc (isLoc:bool) (vE:varEnv) (fE:funcEnv) (sE:structEnv) (aE:argsEnv) (rT:Astype.pjtype) (b:bool) = function
  |[] -> Nothing
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
  let listP = List.fold_right (fun (_, str, _, t) l -> (str, t)::l) pL [] in
  let newdef = chercheDefB true Tset.empty eL in
  let varSet = Tmap.map (fun (k,t) -> t) newdef in
  let varSet = List.fold_left (fun m (str, t) -> Tmap.add str t m) varSet listP in 
  let vE0 = Tmap.filter (fun k _ -> not (Tset.mem k newdef)) vE in
  let vE1 = List.fold_left (fun (Param m (str, t)) -> if exists t sE then Tmap.add str (true, t) m else raise Ast.Typing_Error) vE0 listP in
  let vE2 = parcoursBloc true vE1 fE aE sE eL in
  let vE3 = List.fold_left (fun (Param m (str, t)) -> if exists t sE then Tmap.add str (true, t) m else raise Ast.Typing_Error) vE2 listP in
  let (tb, bt) = testTypEBloc true vE3 fE sE aE pjT true eL in
  if compatible pjT tb
  then if Tmap.mem str fonctions
    then 
      let imap = Tmap.find str fonctions in
      let imap2 = Imap.add (Imap.cardinal imap) (Funct (listP, varSet, (tb, bt)))) in
      Tmap.add str imap2 fonctions
    else Tmap.add str (Imap.singleton 0 (Funct (listP, varSet, (tb, bt)))) fonctions
  else error ("last intruction not compatible "^typeName lt^"-"^typeName pjT) pb

(* effectue le deuxième parcours *)
let rec parcours2 (vEnv:varEnv) (fEnv:funcEnv) (sEnv:structEnv) (aEnv:argsEnv) (fonctions:funcMap)= function
  |[] ->  (vEnv, fEnv, sEnv, aEnv, [], fonctions)
  |Dstruct (b, position, ident, pL)::tl ->
    let fonctions2 = 
      let pL = List.fold_right (fun (_, str, _, t) l -> (str, t)::l) pL [] in
      if Tmap.mem ident fonctions then 
        let imap = Tmap.find str fonctions in
        let imap2 = Imap.add (Imap.cardinal imap) (StructBuilder pL) in
        Tmap.add str imap2 fonctions
      else Tmap.add ident (Imap.singleton 0 (StructBuilder pL) fonctions
    in parcours2 vEnv fEnv sEnv aEnv fonctions2 tl
  |Dfonction  (a, str, b, c, d, e, _)::tl ->
    let fonctions2 = testTypageF vEnv fEnv sEnv aEnv (a, str, b, c, d, e) fonctions
    in parcours2 vEnv fEnv sEnv aEnv fonctions2 tl
  |Dexpr (_, e)::tl ->
    let (t, et) = testTypageE false vEnv fEnv sEnv aEnv Any false e in
    let (a, b, c, d, eL, f) = parcours2 vEnv fEnv sEnv aEnv fonctions tl in
    (a, b, c, d, e::eL, f)

(* fonction globale de vérification du type fait d'utiliser des réfenrence est nécessaire au bon fonctionnement du REPL *)
let verificationType (declL:Ast.fichier) (envV:varEnv ref) (envF:funcEnv ref) (envS:structEnv ref) (envA:argsEnv ref):fichierTyper =
    let DeclarationList dl = declL in
    let vE, fE,sE,aE = parcours1 !envV !envF !envS !envA dl in
    let vp, fp, sp, ap, eL, fonctions = parcours2 (Tmap.add "nothing" (false, Nothing) vE) fE sE aE dl in
    envV := vp;
    envF := fp;
    envS := sp;
    envA := ap;
    (eL, Tmap.map (fun (_, t) -> t) vp, sp, fonctions)

(* fonctions d'interface avec l'extérieur *)
let typerCompilateur = (fun a -> estCompile := true; verificationType a)
let typerRepl = (fun a -> estCompile := false; verificationType a)
