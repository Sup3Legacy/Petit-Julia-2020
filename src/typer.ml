open Ast
open Astype
open Lexer
open Parser

module Tmap = Map.Make(String) (* Map contenant les environnements typage *)
module Tset = Set.Make(String)

type varEnv = Astype.pjtype Tmap.t
type funcEnv = funct Tmap.t
type structEnv = Astype.pjtype Tmap.t Tmap.t
type argsEnv = (bool*Astype.pjtype) Tmap.t

let compatible t1 t2 = t1 = Any || t2 = Any || t1 = t2


let emptyVenv = Tmap.singleton "nothing" Nothing 
let emptyFenv = Tmap.singleton "div" ([Int64; Int64], Int64)
let emptySenv = Tmap.empty
let emptyAenv = Tmap.empty

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

let parcoursStruct sE aE (b,str,l) = 
    let rec aux m a = function 
      |[] -> (m, a)
      |None::tl -> aux m a tl
      |Some (Param (i,t))::tl -> begin
        if Tmap.mem i a then raise Ast.Typing_Error;
        if Tmap.mem i m then raise Ast.Typing_Error;
        aux (Tmap.add i t m) (Tmap.add i (b,t) a) tl
        end
    in let (ajout,aE2) = aux Tmap.empty aE l in
    (Tmap.add str ajout sE, aE2)

let parcoursFonction fE (str, pL, pjT, _) = 
  let tL = List.fold_right (fun (Param (str,t)) l -> t::l) pL [] in
  Tmap.add str (tL, pjT) fE

let rec parcoursExpr vE = function
  | Eentier _ | Echaine _ | Etrue _ | Efalse _ | EentierIdent _ -> vE
  | EentierParG (_,_,b) | Ebloc1 b -> parcoursBloc1 vE b
  | EparDIdent (p,e,str) -> parcoursExpr vE e
  | Eapplication (p,str,eL) -> List.fold_left (fun ve e -> parcoursExpr ve e) vE eL
  | Enot e | Eminus e -> parcoursExpr vE e
  | Ebinop (o,e1,e2) -> parcoursExpr (parcoursExpr vE e1) e2
  | Elvalue _ -> vE
  | ElvalueAffect (Lident str,e) -> Tmap.add str Any (parcoursExpr vE e)
  | ElvalueAffect (_,e) -> parcoursExpr vE e
  | Ereturn None -> vE
  | Ereturn (Some e) -> parcoursExpr vE e
  | Efor (str,e1,e2,b) ->
      let env1 = parcoursExpr vE e1 in 
      let env2 = parcoursExpr env1 e2 in
      let env3 = Tmap.add str Int64 env2 in
      parcoursBloc env3 b
  | Ewhile (e,b) -> parcoursBloc (parcoursExpr vE e) b
  | Eif (e,b,els) ->
      let env1 = parcoursExpr vE e in 
      let env2 = parcoursBloc env1 b in 
      parcoursElse env2 els
and parcoursBloc1 env (Bloc1 (e,o)) = 
  let env1 = parcoursExpr env e in
  match o with
    | None -> env1
    | Some b -> parcoursBloc env1 b
and parcoursBloc vE (Bloc l) = 
  let rec aux env = function
    |[] -> env 
    |None::tl -> aux env tl
    |Some e::tl -> aux (parcoursExpr env e) tl
  in aux vE l
and parcoursElse env = function 
  |Iend -> env
  |Ielse b -> parcoursBloc env b
  |Ielseif (e,b,els) ->
      let env1 = parcoursExpr env e in
      let env2 = parcoursBloc env1 b in
      parcoursElse env2 els

let rec parcours (vEnv:varEnv) (fEnv:funcEnv) (sEnv:structEnv) (aEnv:argsEnv) = function
  |[] -> (vEnv, fEnv, sEnv, aEnv)
  |Dstruct (Struct (a1,a2,a3))::tl ->
      let s,a = parcoursStruct sEnv aEnv (a1,a2,a3) in
      parcours vEnv fEnv s a tl
  |Dfonction (Function (a,b,c,d))::tl -> 
      let fEnv2 = parcoursFonction fEnv (a,b,c,d) in
      parcours vEnv fEnv2 sEnv aEnv tl
  |Dexpr e::tl ->
      let vEnv2 = parcoursExpr vEnv e in
      parcours vEnv2 fEnv sEnv aEnv tl

let parcoursFichier declL =
    let DeclarationList dl = declL in
    parcours emptyVenv emptyFenv emptySenv emptyAenv dl

