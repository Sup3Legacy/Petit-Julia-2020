open Ast
open Astype
open Lexer
open Parser

module Tmap = Map.Make(String) (* Map contenant les environnements typage *)
module Tset = Set.Make(String)
module TypeSet = Set.Make(struct type t = pjtype let compare = compare end)
module FuncSet = Set.Make(struct type t = funct let compare = compare end)

type varEnv = Astype.pjtype Tmap.t
type funcEnv = funct list Tmap.t
type structEnv = Astype.pjtype Tmap.t Tmap.t
type argsEnv = (bool*Astype.pjtype*string) Tmap.t

let estCompile = ref false

let compatible t1 t2 = t1 = Any || t2 = Any || t1 = t2

let rec compatibleF f1 f2 = match f1, f2 with
  |[],[] -> true
  |h1::t1, h2::t2 -> compatible h1 h2 && compatibleF t1 t2
  |_,_ -> failwith "bad implementation of typer"

let rec compatibleFInL f = function
  |[] -> false
  |hd::tl -> compatibleF f hd || compatibleFInL f tl

let error msg p = raise (Ast.Typing_Error_Msg_Pos (msg,p) )
let errorOld msg = raise (Ast.Typing_Error_Msg msg)


let exists t env = match t with
  | Any | Nothing | Int64 | Bool | String -> true
  | S s -> Tmap.mem s env

let argExists t env = Tmap.mem t env

let typeName t = match t with
  | Int64 -> "Int64"
  | Nothing -> "Nothing"
  | Bool -> "Bool"
  | String -> "String"
  | S s -> "Struct \""^s^"\""
  | Any -> "Any"

let parcoursStruct sE (aE:argsEnv) fE (b,p,str,l) =
  if str = "print" || str = "println" || str = "div" then 
    error (str^" is not an allowed function name") p
  else
    if Tmap.mem str sE then error ("already defined structuture of name :"^str) p
    else
      let rec aux m a = function
        |[] -> (m, a)
        |(Param (p1,i,p2,t))::tl -> begin
          if Tmap.mem i a then error ("already existing param name : "^str) p1;
          if exists t sE
          then aux (Tmap.add i t m) (Tmap.add i (b,t,str) a) tl
          else error ("undefined type "^typeName t) p2
          end
      in let rec tList = function
        |[] -> []
        |Param (_,_,_,t)::tl -> t::tList tl
      in let (ajout,aE2) = aux Tmap.empty aE l in
      let fE2 = if Tmap.mem str fE
        then let liste = Tmap.find str fE in Tmap.add str ((tList l, S str)::liste) fE
      else Tmap.add str [(tList l,S str)] fE
      in (Tmap.add str ajout sE, aE2, fE2)

let parcoursFonction vE fE sE (posStr, nameFunc, pL, posT, pjT, _) =
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
            else Tmap.add nameFunc ((tL,pjT)::l1) fE
          else Tmap.add nameFunc [tL, pjT] fE
        else error ("undefined type : "^typeName pjT^" in function "^nameFunc) posT


let rec chercheDefE isFunc (vS:Tset.t) = function
  | Eentier _ | Echaine _ | Etrue | Efalse | EentierIdent _ -> vS
  | EentierParG (_, _, (_, eL)) | Ebloc1 (_, eL) -> chercheDefB isFunc vS eL
  | EparDIdent ((_, e), _, _) -> chercheDefE isFunc vS e
  | Eapplication (_, _, eL) -> List.fold_left (fun env (_,e) -> chercheDefE isFunc env e) vS eL
  | Enot (_, e) | Eminus (_, e) -> chercheDefE isFunc vS e
  | Ebinop (_, _, (_, e1), (_, e2)) -> chercheDefE isFunc (chercheDefE isFunc vS e1) e2
  | Elvalue lv -> begin
    match lv with
      |Lident _ -> vS
      |Lindex ((_, e), _, _) -> chercheDefE isFunc vS e
    end
  | ElvalueAffect (_, Lident (_, str), (_, e)) -> chercheDefE isFunc (Tset.add str vS) e
  | ElvalueAffect (_, Lindex ((_, e1), _, _), (_, e2)) ->
    chercheDefE isFunc (chercheDefE isFunc vS e1) e2
  | Ereturn (_, None) -> vS
  | Ereturn (_, Some (_, e)) -> chercheDefE isFunc vS e
  | Efor (_, (_, e1), (_, e2), (_, eL)) ->
      let env1 = chercheDefE isFunc (chercheDefE isFunc vS e1) e2 in
      if isFunc then chercheDefB isFunc env1 eL else env1
  | Ewhile ((_, e), (_, eL)) ->
      let env1 = chercheDefE isFunc vS e in
      if isFunc then chercheDefB isFunc env1 eL else env1
  | Eif ((_, e), (_, eL), els) ->
      let env1 = chercheDefE isFunc vS e in
      let env2 = chercheDefB isFunc env1 eL in
      chercheDefElse isFunc env2 els
and chercheDefB isFunc (vS:Tset.t) = function
  |[] -> vS
  |(_, e)::tl -> chercheDefB isFunc (chercheDefE isFunc vS e) tl
and chercheDefElse isFunc (vS:Tset.t) = function
  |Iend -> vS
  |Ielse (_, eL) -> chercheDefB isFunc vS eL
  |Ielseif ((_, e), (_, eL), els) ->
      let env1 = chercheDefE isFunc vS e in
      let env2 = chercheDefB isFunc env1 eL in
      chercheDefElse isFunc env2 els

let rec parcoursExpr isFunc vE fE (aE:argsEnv) (sE:structEnv) = function
  | Eentier _ | Echaine _ | Etrue | Efalse | EentierIdent _ -> vE
  | EentierParG (_, _, (_, eL)) | Ebloc1 (_, eL) -> parcoursBloc isFunc vE fE aE sE eL
  | EparDIdent ((_, e), p, str) -> 
    if Tmap.mem str vE then parcoursExpr isFunc vE fE aE sE e
  else error ("undefined variable name "^str) p
  | Eapplication (pStr, str, eL) ->
      if Tmap.mem str sE || Tmap.mem str fE || str = "print" || str = "println"
      then List.fold_left (fun ve (p, e) -> parcoursExpr isFunc ve fE aE sE e) vE eL
      else error ("undefined function 1 "^str) pStr
  | Enot (_, e) | Eminus (_, e) -> parcoursExpr isFunc vE fE aE sE e
  | Ebinop (_, _, (_, e1), (_, e2)) -> parcoursExpr isFunc (parcoursExpr isFunc vE fE aE sE e1) fE aE sE e2
  | Elvalue lv -> begin
    match lv with
      |Lident (p, str) -> if Tmap.mem str vE then vE else error ("undefined variable name "^str) p
      |Lindex ((_, e), p, str) -> let env1 = parcoursExpr isFunc vE fE aE sE e in
        if Tmap.mem str aE then env1 else error ("undefined attribute name "^str) p
    end
  | ElvalueAffect (_, Lident (_, str), (_, e)) ->
    if Tmap.mem str vE && isFunc then vE
    else parcoursExpr isFunc (Tmap.add str Any vE) fE aE sE e
  | ElvalueAffect (_, Lindex ((_, e1), p, str), (_, e2)) ->
    if argExists str aE then
        let env1 = parcoursExpr isFunc vE fE aE sE e1 in
        parcoursExpr isFunc env1 fE aE sE e2
    else error ("undefined attribute name "^str) p
  | Ereturn (p,None) -> vE
  | Ereturn (p, Some (_, e)) -> parcoursExpr isFunc vE fE aE sE e
  | Efor (str, (_, e1), (_, e2), (_, eL)) ->
      let env1 = parcoursExpr isFunc vE fE aE sE e1 in
      let env2 = parcoursExpr isFunc env1 fE aE sE e2 in
      let env3 = Tmap.add str Int64 env2 in
      let env4 = if isFunc then env3
        else (let newdef = chercheDefB isFunc Tset.empty eL in
          Tmap.filter (fun k t ->not (Tset.mem k newdef)) env3)
      in let _ = parcoursBloc isFunc env4 fE aE sE eL
      in vE
  | Ewhile ((_, e), (_, eL)) ->
      let env1 = parcoursExpr isFunc vE fE aE sE e in
      let env2 = if isFunc then env1
        else (let newdef = chercheDefB isFunc Tset.empty eL in
          Tmap.filter (fun k t -> not (Tset.mem k newdef)) env1)
      in let _ = parcoursBloc isFunc env2 fE aE sE eL 
      in vE
  | Eif ((_, e), (_, eL), els) ->
      let env1 = parcoursExpr isFunc vE fE aE sE e in
      let env2 = parcoursBloc isFunc env1 fE aE sE eL in
      parcoursElse isFunc env2 fE aE sE els
and parcoursBloc isFunc vE fE aE sE = function
  |[] -> vE
  |(_, e)::tl -> parcoursBloc isFunc (parcoursExpr isFunc vE fE aE sE e) fE aE sE tl
and parcoursElse isFunc vE fE aE sE = function
  |Iend -> vE
  |Ielse (_, eL) -> parcoursBloc isFunc vE fE aE sE eL
  |Ielseif ((_, e), (_, eL), els) ->
      let env1 = parcoursExpr isFunc vE fE aE sE e in
      let env2 = parcoursBloc isFunc env1 fE aE sE eL in
      parcoursElse isFunc env2 fE aE sE els

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

let rec testTypageE isFunc vE fE sE aE rT b = function
  | Eentier _ -> Int64
  | Echaine _ -> String
  | Etrue | Efalse -> Bool
  | EentierIdent (p, _, str) ->
      let var =
        try Tmap.find str vE
        with _ -> error ("undefined variable name " ^ str) p
      in
      if compatible Int64 var
      then Int64
    else error ("not compatible Int64 with "^typeName (Tmap.find str vE)) p
  | EentierParG (_, _, (pb, eL)) ->
      let t = (testTypEBloc isFunc vE fE sE aE rT b eL) in
      if compatible Int64 t
      then Int64
      else error ("not compatible Int64 with "^typeName t) pb
  | Ebloc1 (_,eL) -> testTypEBloc isFunc vE fE sE aE rT b eL
  | EparDIdent ((pE, e), pI, ident) ->
    if compatible Int64 (try Tmap.find ident vE with _ -> error ("undefined variable name " ^ ident) pI) then
      let t = (testTypageE isFunc vE fE sE aE rT b e) in
      if compatible Int64 t
        then Int64
        else error ("not compatible Int64 with"^typeName t) pE
    else error ("not compatible Int64 with variable "^ident^" of type"^typeName (Tmap.find ident vE)) pI
  | Eapplication (pName, ident, eL) -> begin
    if ident = "print" || ident = "println"
    then
      let () = List.iter (fun (_, e) ->
        let _ = testTypageE isFunc vE fE sE aE rT b e in ()) eL in Nothing
    else
      if Tmap.mem ident fE then
        let l = Tmap.find ident fE in
        let rec aux l1 l2 = match l1,l2 with
          |[],[] -> (0,true)
          |t::tl1, (_, e)::tl2 -> begin
            let t2 = testTypageE isFunc vE fE sE aE rT b e in
            let (nb, b) = aux tl1 tl2 in
            if b && compatible t t2
            then if t=t2 then (nb+1, true) else (nb, true)
            else (0, false)
            end
          | _, _ -> (0, false)
        in
        let (tSet, score, nb, fL) =
          List.fold_left (fun (s,best,nb, fL)  (pL,pjT) ->
                    let (n,b) = aux pL eL in
                    if b then
                      if n = best then ( (TypeSet.add pjT s), n, nb+1, pL::fL)
                      else
                        if n < best
                        then (s, best, nb, fL)
                        else ( (TypeSet.singleton pjT), n, 1,[pL])
                    else (s, best, nb, fL)
                  )
                  (TypeSet.empty, 0, 0, [])
                  l in
        if nb = 1
        then if TypeSet.cardinal tSet = 1 then TypeSet.choose tSet
          else Any
        else if nb > 1 then
          if not b && fst (List.fold_left (fun (b,l1) hd ->
              if b then (true,[])
              else
                  if compatibleFInL hd l1
                  then (true,[])
                  else (false,hd::l1)
              ) (false,[]) fL)
          then error ("too many compatible functions for "^ident) pName
          else
            if TypeSet.cardinal tSet = 1 then TypeSet.choose tSet
            else Any
          else error ("no compatible function for "^ident) pName
       else error ("undeclared function "^ident) pName
        end
  | Enot (p, e) ->
    let t = testTypageE isFunc vE fE sE aE rT b e in
    if compatible Bool t
    then Bool
    else error ("incompatibility of type in Not "^typeName t) p
  | Eminus (p, e) ->
    let t = testTypageE isFunc vE fE sE aE rT b e in
    if compatible Int64 t
    then Int64
    else error ("incompatibility of type in Minus "^typeName t) p
  | Ebinop (p, o, (p1, e1), (p2, e2)) -> begin
    let t1 = testTypageE isFunc vE fE sE aE rT b e1 in
    let t2 = testTypageE isFunc vE fE sE aE rT b e2 in
    match o with
      |Eq | Neq -> Bool
      |Lo | Gr | Leq | Geq ->
        if (compatible t1 Bool || compatible t1 Int64)
            && (compatible t2 Bool || compatible t2 Int64)
        then Bool else error ("not compatible in comparison : "^typeName t1^"!="^typeName t2) p
      |And | Or ->
        if compatible t1 Bool
        then if compatible t2 Bool
          then Bool
          else error ("expected a Bool but got a "^typeName t2) p2
        else error ("expected a Bool but got a "^typeName t1) p1
      | Plus | Minus | Times | Modulo | Exp ->
        if compatible t1 Int64
        then if compatible t2 Int64
          then Int64
          else error ("expected an Int64 but got a "^typeName t2) p2
        else error ("expected an Int64 but got a "^typeName t1) p1
    end
  | Elvalue lv -> begin
    match lv with
      |Lident (p,str) -> Tmap.find str vE
      |Lindex ((_, e), p, n) ->
        let (b,t2, nm) = Tmap.find n aE in
        let t3 = testTypageE isFunc vE fE sE aE rT b e in
        if compatible t3 (S nm) then t2 else error ("type incompatibility in index "^typeName t3^" not compatible with struct "^nm) p
    end
  | ElvalueAffect (pEqual, lv, (pe, e)) -> begin
    let t = testTypageE isFunc vE fE sE aE rT b e in
    match lv with
      | Lident (p, str) ->
        if Tmap.mem str fE then error (str^" is also a function, can't be both") p
        else
          let t2 = Tmap.find str vE in
          if compatible t t2 then t
          else error ("type incompatibility in affectation : "^typeName t^" can't be given to "^str^" who has type "^typeName t2) pEqual
      | Lindex ((pe2, e2), pDot, n) ->
        let (b, t2, nm) = Tmap.find n aE in
        if b then
          let t3 = testTypageE isFunc vE fE sE aE rT b e2 in
          if compatible t3 (S nm)
          then if compatible t t2
            then t
            else error ("type incompatibility in index affectation : "^typeName t^" can't be given to var who has type "^typeName t2) pEqual
          else error ("type incompatibility in index affectation : "^typeName t3^" is not compatible with Struct "^nm) pDot
        else error ("type incompatibility in index affectation : "^nm^" who has "^n^" as an attribute is not mutable") pDot
    end
  | Ereturn (p, opt) -> if b
    then begin
      match opt with
        | None -> if compatible rT Nothing then Any else error ("Expected a "^typeName rT^" but found a Nothing") p
        | Some (pe, e) -> let t = testTypageE isFunc vE fE sE aE rT b e in
          if compatible rT t then Any
          else  error ("Expected a "^typeName rT^" but found a "^typeName t) pe
      end
    else error "Returns must appear inside functions" p
  | Efor (i, (p1, e1), (p2, e2), (pb, eL)) ->
    let vE1 = parcoursExpr isFunc vE fE aE sE e1 in
    let vE2 = parcoursExpr isFunc vE1 fE aE sE e2 in
    let vE3 = parcoursBloc isFunc (Tmap.add i Int64 vE2) fE aE sE eL in
    let t1 = testTypageE isFunc vE1 fE sE aE rT b e1 in
    if compatible t1 Int64
    then let t2 = testTypageE isFunc vE2 fE sE aE rT b e2 in
      if compatible t2 Int64
      then let _ = testTypEBloc isFunc vE3 fE sE aE rT b eL in Nothing
      else error ("expected an Int64 but got an "^typeName t2) p2
    else error ("expected an Int64 but got an "^typeName t1) p1
  | Ewhile ((pe, e), (pb, eL)) ->
    let vE = parcoursBloc isFunc vE fE aE sE eL in
    let t = testTypageE isFunc vE fE sE aE rT b e in
    if compatible Bool t
    then let _ = testTypEBloc isFunc vE fE sE aE rT b eL in Nothing
    else error ("expected a Bool but got an "^typeName t) pe
  | Eif ((pe, e), (pb, eL), els) ->
      let t = testTypageE isFunc vE fE sE aE rT b e in
      if compatible Bool t
      then let t1 = testTypEBloc isFunc vE fE sE aE rT b eL in
        begin
          match testTypEElse isFunc vE fE sE aE rT b els with
            |None -> t1
            |Some t2 -> if t1 = t2 then t1 else Any
        end
      else error ("expected a Bool but got an "^typeName t) pe
and testTypEBloc isFunc vE fE sE aE rT b = function
  |[] -> Nothing
  |[(p,e)] -> testTypageE isFunc vE fE sE aE rT b e
  |(p,e)::tl ->
      let _ = testTypageE isFunc vE fE sE aE rT b e
      in testTypEBloc isFunc vE fE sE aE rT b tl
and testTypEElse isFunc vE fE sE aE rT b = function
  |Iend -> None
  |Ielse (pb, eL) -> Some (testTypEBloc isFunc vE fE sE aE rT b eL)
  |Ielseif ((pe, e), (pb, eL), els) ->
    let te = testTypageE isFunc vE fE sE aE rT b e in
    if compatible te Bool then
      let t1 = testTypEBloc isFunc vE fE sE aE rT b eL in
      begin
        match testTypEElse isFunc vE fE sE aE rT b els with
          |None -> Some t1
          |Some t2 -> if t1 = t2 then Some t1 else Some Any
      end
    else error ("expected a Bool but got a "^typeName te) pe

let rec containsNotRetBloc = function
  | [] -> true
  | (_,e)::tl -> containsNotRetE e && containsNotRetBloc tl
and containsNotRetE e = match e with
  | Eentier _ | Echaine _ | Etrue | Efalse | EentierIdent _ -> true
  | EentierParG (_, _, (_, eL)) | Ebloc1 (_, eL) -> containsNotRetBloc eL
  | EparDIdent ((_, e), _, _) | Enot (_, e) | Eminus (_, e) -> containsNotRetE e
  | Eapplication (_, _, eL) -> List.fold_left (fun b (_, e) -> b && containsNotRetE e) true eL
  | Ebinop (_, _, (_, e1), (_, e2)) -> containsNotRetE e1 && containsNotRetE e2
  | Elvalue lv -> begin
    match lv with
    | Lident _ -> true
    | Lindex ((_, e), _, _) -> containsNotRetE e
    end
  | ElvalueAffect (_, lv, (_, e)) -> containsNotRetE e && (match lv with
    | Lident _ -> true
    | Lindex ((_, e2), _, _) -> containsNotRetE e2)
  | Ereturn _ -> false
  | Efor (_, (_, e1), (_, e2), (_, eL)) -> containsNotRetE e1 && containsNotRetE e2 && containsNotRetBloc eL
  | Ewhile ((_, e), (_, eL)) -> containsNotRetE e && containsNotRetBloc eL
  | Eif ((_, e), (_, eL), els) -> containsNotRetE e && containsNotRetBloc eL && containsNotRetElse els
and containsNotRetElse els = match els with
  | Iend -> true
  | Ielse (_, eL) -> containsNotRetBloc eL
  | Ielseif ((_, e), (_, eL), els) -> containsNotRetE e && containsNotRetBloc eL && containsNotRetElse els

let rec lastInstruction v = function
  |[] -> v
  |v2::tl -> lastInstruction v2 tl

let testTypageF (vE:varEnv) (fE:funcEnv) (sE:structEnv) (aE:argsEnv) (posN, str, pL, posT, pjT, (pb, eL)) =
  let newdef = chercheDefB true Tset.empty eL in
  let vE0 = Tmap.filter (fun k _ -> not (Tset.mem k newdef)) vE in
  let vE1 = List.fold_right (fun (Param (_, str, _, t)) m -> if exists t sE then Tmap.add str t m else raise Ast.Typing_Error) pL vE0 in
  let vE2 = parcoursBloc true vE1 fE aE sE eL in
  let vE3 = List.fold_right (fun (Param (_, str,_, t)) m -> if exists t sE then Tmap.add str t m else raise Ast.Typing_Error) pL vE2 in
  let _ = testTypEBloc true vE3 fE sE aE pjT true eL in
  let (pe, lastE) = lastInstruction ({ldeb = 0;cdeb = 0; lfin = 0; cfin = 0}, Elvalue (Lident ({ldeb = 0;cdeb = 0; lfin = 0; cfin = 0}, "nothing"))) eL in
  let lt = testTypageE true vE3 fE sE aE pjT true lastE in
  if compatible pjT lt
  then ()
  else error ("last intruction not compatible "^typeName lt^"-"^typeName pjT) pe


let rec parcours2 (vEnv:varEnv) (fEnv:funcEnv) (sEnv:structEnv) (aEnv:argsEnv) = function
  |[] ->  (vEnv, fEnv, sEnv, aEnv)
  |Dstruct _::tl -> parcours2 vEnv fEnv sEnv aEnv tl
  |Dfonction  (a, str, b, c, d, e, _)::tl -> let () = (try testTypageF vEnv fEnv sEnv aEnv (a, str, b, c, d, e)  with Not_found -> (print_string ("- f "^str^" -");print_newline ();raise Not_found)) in parcours2 vEnv fEnv sEnv aEnv tl
  |Dexpr (_, e)::tl -> let _ = (try testTypageE false vEnv fEnv sEnv aEnv Any false e with Not_found -> (print_string "- e -";print_newline ();raise Not_found)) in parcours2 vEnv fEnv sEnv aEnv tl


let verificationType declL envV envF envS envA=
    let DeclarationList dl = declL in
    let vE, fE,sE,aE = parcours1 !envV !envF !envS !envA dl in
    let vp, fp, sp, ap = parcours2 (Tmap.add "nothing" Nothing vE) fE sE aE dl in
    envV := vp;
    envF := fp;
    envS := sp;
    envA := ap

let typerCompilateur = (fun a -> estCompile := true; verificationType a)
let typerRepl = (fun a -> estCompile := false; verificationType a)
