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

let compatible t1 t2 = t1 = Any || t2 = Any || t1 = t2

let rec compatibleF f1 f2 = match f1, f2 with
  |[],[] -> true
  |h1::t1, h2::t2 -> compatible h1 h2 && compatibleF t1 t2
  |_,_ -> failwith "bad implementation of typer"

let rec compatibleFInL f = function
  |[] -> false
  |hd::tl -> compatibleF f hd || compatibleFInL f tl

let error msg = raise (Ast.Typing_Error_Msg msg)

(*
let rec type_fonctions prog (env : funcEnv) =
  (* retourne l'environnement contenant les déclaratiosn de type des fonctions, sans les explorer *)
  match prog with
  | DeclarationList [] -> env
  | DeclarationList (t :: q) -> type_fonctions (DeclarationList q) (add_function t env)
;;

let add_function func (env : funcEnv) =
  match func with
  | Function (i, p, t, b) ->
    if Tmap.mem i env then
      Tmap.add i (t :: (Tmap.find i env)) env
    else
      Tmap.add i [t] env
;;*)

let exists t env = match t with
  |Any | Nothing | Int64 | Bool | String -> true
  | S s -> Tmap.mem s env

let argExists t env = Tmap.mem t env

let typeName t = match t with
  |Int64 -> "int"
  |Nothing -> "nothing"
  |Bool -> "bool"
  |String -> "string"
  |S s -> "struct : "^s
  |Any -> "any"

let parcoursStruct sE (aE:argsEnv) (b,str,l) =
  if Tmap.mem str sE then error ("existing already struct :"^str)
  else
    let rec aux m a = function
      |[] -> (m, a)
      |None::tl -> aux m a tl
      |Some (Param (i,t))::tl -> begin
        if Tmap.mem i a then error "already existing param name";
        if exists t sE then aux (Tmap.add i t m) (Tmap.add i (b,t,str) a) tl else error ("undefined type "^typeName t)
        end
    in let (ajout,aE2) = aux Tmap.empty aE l in
    (Tmap.add str ajout sE, aE2)

let parcoursFonction fE sE (str, pL, pjT, _) =
  let (tL,tS) = List.fold_right
    (fun (Param (str,t)) (l,tSet) -> if exists t sE then (t::l,Tset.add str tSet) else error ("undefined type : "^typeName t^" in function "^str))
    pL
    ([],Tset.empty)
  in
  if Tset.cardinal tS <> List.length pL
  then error ("redundant argument name")
  else
    if exists pjT sE then
      if Tmap.mem str fE then
        let l1 = Tmap.find str fE in
        let rec estDedans a = function
          |[] -> false
          |hd::tl -> (hd=a)||estDedans a tl
        in if estDedans (tL,pjT) l1 then error ("already exiting function")
        else Tmap.add str ((tL,pjT)::l1) fE
      else Tmap.add str [tL, pjT] fE
    else error ("undefined type : "^typeName pjT^" in function "^str)

let rec parcoursExpr vE fE (aE:argsEnv) (sE:structEnv) = function
  | Eentier _ | Echaine _ | Etrue _ | Efalse _ | EentierIdent _ -> vE
  | EentierParG (_,_,b) | Ebloc1 b -> parcoursBloc1 vE fE aE sE b
  | EparDIdent (p,e,str) -> parcoursExpr vE fE aE sE e
  | Eapplication (p,str,eL) ->
      if Tmap.mem str sE || Tmap.mem str fE || str = "print" || str = "println"
      then List.fold_left (fun ve e -> parcoursExpr ve fE aE sE e) vE eL
      else error ("undefined function 1 "^str)
  | Enot e | Eminus e -> parcoursExpr vE fE aE sE e
  | Ebinop (o,e1,e2) -> parcoursExpr (parcoursExpr vE fE aE sE e1) fE aE sE e2
  | Elvalue (_,lv) -> begin
    match lv with
      |Lident str -> if Tmap.mem str vE then vE else error ("undefined var 1 "^str)
      |Lindex (e,str) -> let env1 = parcoursExpr vE fE aE sE e in
        if Tmap.mem str aE then env1 else error ("undefined attribute 1 "^str)
    end
  | ElvalueAffect (Lident str,e) ->
      parcoursExpr (Tmap.add str Any vE) fE aE sE e
  | ElvalueAffect (Lindex (e1,str),e2) ->
    if argExists str aE then
        let env1 = parcoursExpr vE fE aE sE e1 in
        parcoursExpr env1 fE aE sE e2
    else error ("undefined attribute 2 "^str)
  | Ereturn None -> vE
  | Ereturn (Some e) -> parcoursExpr vE fE aE sE e
  | Efor (str,e1,e2,b) ->
      let env1 = parcoursExpr vE fE aE sE e1 in
      let env2 = parcoursExpr env1 fE aE sE e2 in
      let env3 = Tmap.add str Int64 env2 in
      let _ = parcoursBloc env3 fE aE sE b in
      vE
  | Ewhile (e,b) ->
      let _ = parcoursBloc (parcoursExpr vE fE aE sE e) fE aE sE b
      in vE
  | Eif (e,b,els) ->
      let env1 = parcoursExpr vE fE aE sE e in
      let env2 = parcoursBloc env1 fE aE sE b in
      parcoursElse env2 fE aE sE els
and parcoursBloc1 env fE aE sE (Bloc1 (e,o)) =
  let env1 = parcoursExpr env fE aE sE e in
  match o with
    | None -> env1
    | Some b -> parcoursBloc env1 fE aE sE b
and parcoursBloc vE fE aE sE (Bloc l) =
  let rec aux env = function
    |[] -> env
    |None::tl -> aux env tl
    |Some e::tl -> aux (parcoursExpr env fE aE sE e) tl
  in aux vE l
and parcoursElse env fE aE sE = function
  |Iend -> env
  |Ielse b -> parcoursBloc env fE aE sE b
  |Ielseif (e,b,els) ->
      let env1 = parcoursExpr env fE aE sE e in
      let env2 = parcoursBloc env1 fE aE sE b in
      parcoursElse env2 fE aE sE els

let rec parcours1 (vEnv:varEnv) (fEnv:funcEnv) (sEnv:structEnv) (aEnv:argsEnv) = function
  |[] -> (vEnv, fEnv, sEnv, aEnv)
  |Dstruct (Struct (a1,a2,a3))::tl -> begin
      let s,a = parcoursStruct sEnv aEnv (a1,a2,a3) in
      parcours1 vEnv fEnv s a tl
      end
  |Dfonction (Function (a,b,c,d))::tl -> begin
      let fEnv2 = parcoursFonction fEnv sEnv (a,b,c,d) in
      parcours1 vEnv fEnv2 sEnv aEnv tl
      end
  |Dexpr e::tl -> begin
      let vEnv2 = parcoursExpr vEnv fEnv aEnv sEnv e in
      parcours1 vEnv2 fEnv sEnv aEnv tl
      end

let rec testTypageE vE fE sE aE = function
  | Eentier _ -> Int64
  | Echaine _ -> String
  | Etrue _ | Efalse _ -> Bool
  | EentierIdent (_,_,str) ->
      if compatible Int64 (Tmap.find str vE)
      then Int64
    else error ("not compatible 1 int-"^typeName (Tmap.find str vE))
  | EentierParG (_,_,b1) ->
      let t = (testTypEBloc1 vE fE sE aE b1) in
      if compatible Int64 t
      then Int64
      else error ("not compatible 2 int-"^typeName t)
  | Ebloc1 b1 -> testTypEBloc1 vE fE sE aE b1
  | EparDIdent (_,e,ident) -> if compatible Int64 (Tmap.find ident vE) then
        let t = (testTypageE vE fE sE aE e) in
        if compatible Int64 t
        then Int64
        else error ("not compatible 3 int-"^typeName t)
      else error ("not compatible 4 int-"^typeName (Tmap.find ident vE))
  | Eapplication (_,ident,eL) -> begin
    if ident = "print" || ident = "println"
    then
      let () = List.iter (fun e -> let _ = testTypageE vE fE sE aE e in()) eL in Nothing
    else
      if Tmap.mem ident fE then
        let l = Tmap.find ident fE in
        let rec aux l1 l2 = match l1,l2 with
          |[],[] -> (0,true)
          |t::tl1, e::tl2 -> begin
            let t2 = testTypageE vE fE sE aE e in
            let (nb,b) = aux tl1 tl2 in
            if b && compatible t t2
            then if t=t2 then (nb+1,true) else (nb,true)
            else (0,false)
            end
          | _, _ -> (0,false)
        in
        let (tSet,score,nb, fL) =
          List.fold_left (fun (s,best,nb, fL)  (pL,pjT) ->
                    let (n,b) = aux pL eL in
                    if b then
                      if n=best then ( (TypeSet.add pjT s) ,n,nb+1, pL::fL)
                      else
                        if n<best
                        then (s, best, nb, fL)
                        else ( (TypeSet.singleton pjT) ,n,1, [pL])
                    else (s,best,nb, fL)
                  )
                  (TypeSet.empty,0,0, [])
                  l in
        if nb = 1
        then if TypeSet.cardinal tSet = 1 then TypeSet.choose tSet
          else Any
        else if nb > 1 then
          if fst (List.fold_left (fun (b,l1) hd ->
              if b then (true,[])
              else
                  if compatibleFInL hd l1
                  then (true,[])
                  else (false,hd::l1)
              ) (false,[]) fL)
          then error ("too many compatible functions for "^ident)
          else
            if TypeSet.cardinal tSet = 1 then TypeSet.choose tSet
            else Any
          else error ("no compatible function")
       else
        if Tmap.mem ident sE
        then S ident
        else error ("undeclared function "^ident)
        end
  | Enot e ->
    let t = testTypageE vE fE sE aE e in
    if compatible Bool t
    then Bool
    else error ("incompatibility of type in a not "^typeName t)
  | Eminus e ->
    let t = testTypageE vE fE sE aE e in
    if compatible Int64 t
    then Int64
    else error ("incompatibility of type in a minus "^typeName t)
  | Ebinop (o,e1,e2) -> begin
    let t1 = testTypageE vE fE sE aE e1 in
    let t2 = testTypageE vE fE sE aE e2 in
    match o with
      |Eq | Neq -> Bool
      |Lo | Gr | Leq | Geq ->
        if (compatible t1 Bool || compatible t1 Int64)
            && (compatible t2 Bool || compatible t2 Int64)
        then Bool else error ("not compatible in comparison : "^typeName t1^"!="^typeName t2)
      |And | Or ->
        if compatible t1 Bool && compatible t2 Bool
        then Bool
        else error ("one is not bool in boolean operator "^typeName t1^"-"^typeName t2)
      | Plus | Minus | Times | Modulo | Exp ->
        if compatible t1 Int64 && compatible t2 Int64 then Int64
        else error ("one is not int in arithmetic operator "^typeName t1^"-"^typeName t2)
    end
  | Elvalue (_,lv) -> begin
    match lv with
      |Lident str -> Tmap.find str vE
      |Lindex (e,n) ->
        let (b,t2, nm) = Tmap.find n aE in
        let t3 = testTypageE vE fE sE aE e in
        if compatible t3 (S nm) then t2 else error ("type incompatibility in index "^typeName t3^" not compatible with struct "^nm)
    end
  | ElvalueAffect (lv,e) -> begin
    let t = testTypageE vE fE sE aE e in
    match lv with
      | Lident str ->
        let t2 = Tmap.find str vE in
        if compatible t (Tmap.find str vE) then t
        else error ("type incompatibility in affectation : "^typeName t^" can't be given to "^str^" who has type "^typeName t2)
      | Lindex (e,n) ->
        let (b,t2,nm) = Tmap.find n aE in
        if b then
          let t3 = testTypageE vE fE sE aE e in
          if compatible t3 (S nm) then if compatible t t2 then t
            else error ("type incompatibility in index affectation : "^typeName t^" can't be given to var who has type "^typeName t2)
          else error ("type incompatibility in index affectation : "^typeName t3^" is not compatible with "^nm)
        else error ("type incompatibility in index affectation : "^nm^" who has "^n^" as an attribute is not mutable")
    end
  | Ereturn _ -> error "can't have a return outside of a function"
  | Efor (i,e1,e2,Bloc b) ->
    let vE1 = parcoursExpr vE fE aE sE e1 in
    let vE2 = parcoursExpr vE1 fE aE sE e2 in
    let vE3 = parcoursBloc (Tmap.add i Int64 vE2) fE aE sE (Bloc b) in
    if compatible (testTypageE vE1 fE sE aE  e1) Int64 && compatible (testTypageE vE2 fE sE aE e2) Int64
    then let _ = testTypEBloc vE3 fE sE aE b in Nothing
    else error "type incompatibility in a for"
  | Ewhile (e,Bloc b) ->
    let vE = parcoursBloc vE fE aE sE (Bloc b) in
    let t = testTypageE vE fE sE aE e in
    if compatible Bool t
    then let _ = testTypEBloc vE fE sE aE b in Nothing
    else error ("you need a boolean not a "^typeName t^" as a while condition")
  | Eif (e,Bloc b,els) ->
      let t = testTypageE vE fE sE aE e in
      if compatible Bool t
      then let t1 = testTypEBloc vE fE sE aE b in
        begin
          match testTypEElse vE fE sE aE els with
            |None -> t1
            |Some t2 -> if t1 = t2 then t1 else Any
        end
      else error ("you need a boolean not a "^typeName t^" as a if condition")
and testTypEBloc1 vE fE sE aE = function
  |Bloc1 (e,None) -> testTypageE vE fE sE aE e
  |Bloc1 (e,Some (Bloc b)) ->
      let _ = testTypageE vE fE sE aE e
      in testTypEBloc vE fE sE aE b
and testTypEBloc vE fE sE aE = function
  |[] -> Nothing
  |None::tl -> testTypEBloc vE fE sE aE tl
  |[Some e] -> testTypageE vE fE sE aE e
  |Some e::tl ->
      let _ = testTypageE vE fE sE aE e
      in testTypEBloc vE fE sE aE tl
and testTypEElse vE fE sE aE = function
  |Iend -> None
  |Ielse (Bloc b) -> Some (testTypEBloc vE fE sE aE b)
  |Ielseif (e,Bloc b, els) ->
    let te = testTypageE vE fE sE aE e in
    if compatible te Bool then
      let t1 = testTypEBloc vE fE sE aE b in
      begin
        match testTypEElse vE fE sE aE els with
          |None -> Some t1
          |Some t2 -> if t1 = t2 then Some t1 else Some Any
      end
    else error (" not the good type")

let rec testTypageBlocF vE fE sE aE pjT = function
  |[] -> Nothing
  |None::tl -> testTypageBlocF vE fE sE aE pjT tl
  |[Some e] -> testTypBlocF2 vE fE sE aE pjT e
  |Some e::tl -> let _ = testTypBlocF2 vE fE sE aE pjT e in testTypageBlocF vE fE sE aE pjT tl
and testTypBlocF2 vE fE sE aE pjT = function
  | Eentier _ -> Int64
  | Echaine _ -> String
  | Etrue _ | Efalse _ -> Bool
  | EentierIdent (_,_,str) ->
      let t = (try Tmap.find str vE with Not_found -> error ("undefined var "^str))in
      if compatible Int64 t then Int64
      else error ("type incompatibility in a function :"^typeName t^" of var "^str^" is not compatible with Int")
  | EentierParG (_,_,b1) ->
      let t = testTypFBloc1 vE fE sE aE pjT b1 in
      if compatible Int64 t then Int64
      else error ("type incompatibility in a function :"^typeName t^" is not compatible with Int")
  | Ebloc1 b1 -> testTypFBloc1 vE fE sE aE pjT b1
  | EparDIdent (_,e,ident) ->
      let t1 = (try Tmap.find ident vE with Not_found -> error ("undefined var "^ident)) in
      if compatible Int64 t1 then
        let t2 = testTypBlocF2 vE fE sE aE pjT e in
        if compatible Int64 t2
        then Int64
        else error ("type incompatibility in a function : the expression has type "^typeName t2^" which is incompatible with int")
      else error (" in a function the var "^ident^" has type "^typeName t1^" which is incompatible with int")
  | Eapplication (_,ident,eL) -> begin
    if ident = "print" || ident = "println"
    then
      let () = List.iter (fun e -> let _ = testTypageE vE fE sE aE e in()) eL in Nothing
    else
      if Tmap.mem ident fE then
        let l = Tmap.find ident fE in
        let rec aux l1 l2 = match l1,l2 with
          |[],[] -> (0,true)
          |t::tl1, e::tl2 -> begin
            let t2 = testTypBlocF2 vE fE sE aE pjT e in
            let (nb,b) = aux tl1 tl2 in
            if b && compatible t t2
            then if t=t2 then (nb+1,true) else (nb,true)
            else (0,false)
            end
          | _, _ -> (0,false)
        in
        let (tSet,score,nb) =
          List.fold_left (fun ((s,best,nb):TypeSet.t * int * int)  ((pL,pjT2):funct) ->
                    let (n,b) = aux pL eL in
                    if b then
                      if n=best then ( (TypeSet.add pjT2 s) ,n,nb+1)
                      else
                        if n<best
                        then (s,best,nb)
                        else ( (TypeSet.singleton pjT2) ,n,1)
                    else (s,best,nb)
                  )
                  (TypeSet.empty,0,0)
                  l in
        if nb != 0
        then if TypeSet.cardinal tSet = 1 then TypeSet.choose tSet
          else Any
        else error ("no compatible function for "^ident)
       else
        if Tmap.mem ident sE
        then S ident
        else error ("undeclared function "^ident)
      end
  | Enot e -> if compatible Bool (testTypBlocF2 vE fE sE aE pjT e) then Bool else raise Ast.Typing_Error
  | Eminus e -> if compatible Int64 (testTypBlocF2 vE fE sE aE pjT e) then Int64 else raise Ast.Typing_Error
  | Ebinop (o,e1,e2) -> begin
    let t1 = testTypBlocF2 vE fE sE aE pjT e1 in
    let t2 = testTypBlocF2 vE fE sE aE pjT e2 in
    match o with
      |Eq | Neq -> if compatible t1 t2 then Bool else raise Ast.Typing_Error
      |Lo | Gr | Leq | Geq ->
        if (compatible t1 Bool || compatible t1 Int64)
            && (compatible t2 Bool || compatible t2 Int64)
        then Bool else raise Ast.Typing_Error
      |And | Or ->
        if compatible t1 Bool && compatible t2 Bool
        then Bool
        else raise Ast.Typing_Error
      | Plus | Minus | Times | Modulo | Exp ->
        if compatible t1 Int64 && compatible t2 Int64 then Int64
        else raise Ast.Typing_Error
    end
  | Elvalue (_,lv) -> begin
    match lv with
      |Lident str -> (try Tmap.find str vE with Not_found -> error (str^" l.347"))
      |Lindex (e,n) ->
        let (b,t2, nm) = Tmap.find n aE in
        let t3 = testTypBlocF2 vE fE sE aE pjT e in
        if compatible t3 (S nm) then t2 else raise Ast.Typing_Error
    end
  | ElvalueAffect (lv,e) -> begin
    let t = testTypBlocF2 vE fE sE aE pjT e in
    match lv with
      | Lident i -> if compatible t (try Tmap.find i vE with Not_found -> (print_string ("undefined var l.439 "^i^" in function\n");raise Not_found))then t else raise Ast.Typing_Error
      | Lindex (e,n) ->
        let (b,t2,nm) = (try Tmap.find n aE  with Not_found -> error ("undefined attribute "^n))in
        if b then
          let t3 = testTypBlocF2 vE fE sE aE pjT e in
          if compatible t3 (S nm) then if compatible t t2 then t
            else raise Ast.Typing_Error
          else raise Ast.Typing_Error
        else raise Ast.Typing_Error
    end
  | Ereturn None -> if compatible pjT Nothing then Nothing else raise Ast.Typing_Error
  | Ereturn (Some e) -> if compatible pjT (testTypBlocF2 vE fE sE aE pjT e) then Nothing else raise Ast.Typing_Error
  | Efor (i,e1,e2,Bloc b) ->
    let vE1 = parcoursExpr vE fE aE sE e1 in
    let vE2 = parcoursExpr vE1 fE aE sE e2 in
    let vE3 = parcoursBloc (Tmap.add i Int64 vE2) fE aE sE (Bloc b) in
    if compatible (testTypBlocF2 vE3 fE sE aE pjT e1) Int64 && compatible (testTypBlocF2 vE3 fE sE aE pjT e2) Int64
    then let _ = testTypFBloc (Tmap.add i Int64 vE3) fE sE aE pjT b in Nothing
    else raise Ast.Typing_Error
  | Ewhile (e,Bloc b) ->
    let vE = parcoursBloc vE fE aE sE (Bloc b) in
    let t = testTypageE vE fE sE aE e in
    if compatible Bool t
    then let _ = testTypFBloc vE fE sE aE pjT b in Nothing
    else raise Ast.Typing_Error
  | Eif (e,b,els) ->
      if compatible Bool (testTypBlocF2 vE fE sE aE pjT e)
      then Any
      else raise Ast.Typing_Error
and testTypFBloc1 vE fE sE aE pjT = function
  |Bloc1 (e,None) -> testTypBlocF2 vE fE sE aE pjT e
  |Bloc1 (e,Some (Bloc b)) ->
      let _ = testTypBlocF2 vE fE sE aE pjT e
      in testTypFBloc vE fE sE aE pjT b
and testTypFBloc vE fE sE aE pjT = function
  |[] -> Nothing
  |None::tl -> testTypFBloc vE fE sE aE pjT tl
  |[Some e] -> testTypBlocF2 vE fE sE aE pjT e
  |Some e::tl ->
      let _ = testTypBlocF2 vE fE sE aE pjT e
      in testTypFBloc vE fE sE aE pjT tl
and testTypFElse vE fE sE aE pjT = function
  |Iend -> None
  |Ielse (Bloc b) -> Some (testTypFBloc vE fE sE aE pjT b)
  |Ielseif (e,Bloc b, els) ->
    let te = testTypBlocF2 vE fE sE aE pjT e in
    if compatible te Bool then
      let t1 = testTypFBloc vE fE sE aE pjT b in
      begin
        match testTypFElse vE fE sE aE pjT els with
          |None -> Some t1
          |Some t2 -> if t1 = t2 then Some t1 else Some Any
      end
    else error ("not the good type")

let rec containsNotRetBloc = function
  | [] -> true
  | None::tl -> containsNotRetBloc tl
  | Some e::tl -> containsNotRetE e && containsNotRetBloc tl
and containsNotRetE e = match e with
  | Eentier _ | Echaine _ | Etrue _ | Efalse _ | EentierIdent _ -> true
  | EentierParG (_,_,Bloc1 (e,b)) | Ebloc1 (Bloc1 (e,b)) -> containsNotRetBloc1 (e,b)
  | EparDIdent (_,e,_) | Enot e | Eminus e -> containsNotRetE e
  | Eapplication (_,_,eL) -> List.fold_left (fun b e -> b && containsNotRetE e) true eL
  | Ebinop (o, e1, e2) -> containsNotRetE e1 && containsNotRetE e2
  | Elvalue (_,lv) -> begin
    match lv with
    | Lident _ -> true
    | Lindex (e,_) -> containsNotRetE e
    end
  | ElvalueAffect (lv,e) -> containsNotRetE e && (match lv with
    | Lident _ -> true
    | Lindex (e2,_) -> containsNotRetE e2)
  | Ereturn _ -> false
  | Efor (_,e1,e2,Bloc b) -> containsNotRetE e1 && containsNotRetE e2 && containsNotRetBloc b
  | Ewhile (e,Bloc b) -> containsNotRetE e && containsNotRetBloc b
  | Eif (e,Bloc b, els) -> containsNotRetE e && containsNotRetBloc b && containsNotRetElse els
and containsNotRetBloc1 b1 = match b1 with
  |e,None -> containsNotRetE e
  |e,Some (Bloc b) -> containsNotRetE e && containsNotRetBloc b
and containsNotRetElse els = match els with
  | Iend -> true
  | Ielse (Bloc b) -> containsNotRetBloc b
  | Ielseif (e,Bloc b, els) -> containsNotRetE e && containsNotRetBloc b && containsNotRetElse els

let rec lastInstruction v = function
  |[] -> v
  |None::tl -> lastInstruction v tl
  |Some v2::tl -> lastInstruction v2 tl

let testTypageF (vE:varEnv) (fE:funcEnv) (sE:structEnv) (aE:argsEnv) f =
  let Function (str,pL,pjT, Bloc b) = f in
  let vE1 = List.fold_right (fun (Param (str,t)) m -> if exists t sE then Tmap.add str t m else raise Ast.Typing_Error) pL vE in
  let vE2 = parcoursBloc vE1 fE aE sE (Bloc b) in
  let vE3 = List.fold_right (fun (Param (str,t)) m -> if exists t sE then Tmap.add str t m else raise Ast.Typing_Error) pL vE2 in
  let _ = testTypageBlocF vE3 fE sE aE pjT b in
  if (not (compatible pjT Nothing)) && (containsNotRetBloc b)
  then
    let lt = testTypBlocF2 vE3 fE sE aE pjT (lastInstruction (Elvalue ({ldeb = 0;cdeb = 0; lfin = 0; cfin = 0}, Lident "nothing")) b) in
    if compatible pjT lt
    then ()
    else error ("last intruction not compatible "^typeName lt^"-"^typeName pjT)
  else ()


let rec parcours2 (vEnv:varEnv) (fEnv:funcEnv) (sEnv:structEnv) (aEnv:argsEnv) = function
  |[] ->  (vEnv, fEnv, sEnv, aEnv)
  |Dstruct _::tl -> parcours2 vEnv fEnv sEnv aEnv tl
  |Dfonction  (Function (str,_,_,_) as f)::tl -> let () = (try testTypageF vEnv fEnv sEnv aEnv f  with Not_found -> (print_string ("- f "^str^" -");print_newline ();raise Not_found)) in parcours2 vEnv fEnv sEnv aEnv tl
  |Dexpr e::tl -> let _ = (try testTypageE vEnv fEnv sEnv aEnv e with Not_found -> (print_string "- e -";print_newline ();raise Not_found)) in parcours2 vEnv fEnv sEnv aEnv tl


let verificationType declL envV envF envS envA=
    let DeclarationList dl = declL in
    let vE, fE,sE,aE = parcours1 !envV !envF !envS !envA dl in
    let vp, fp, sp, ap = parcours2 (Tmap.add "nothing" Nothing vE) fE sE aE dl in
    envV := vp;
    envF := fp;
    envS := sp;
    envA := ap;
