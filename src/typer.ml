open Ast
open Astype
open Lexer
open Parser

module Tmap = Map.Make(String) (* Map contenant les environnements typage *)
module Tset = Set.Make(String)

type varEnv = Astype.pjtype Tmap.t
type funcEnv = funct Tmap.t
type structEnv = Astype.pjtype Tmap.t Tmap.t
type argsEnv = (bool*Astype.pjtype*string) Tmap.t

let compatible t1 t2 = t1 = Any || t2 = Any || t1 = t2

let error msg = raise (Ast.Typing_Error_Msg msg)

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
    let rec aux m a = function 
      |[] -> (m, a)
      |None::tl -> aux m a tl
      |Some (Param (i,t))::tl -> begin
        if Tmap.mem i a then error "already existing param name";
        aux (Tmap.add i t m) (Tmap.add i (b,t,str) a) tl
        end
    in let (ajout,aE2) = aux Tmap.empty aE l in
    (Tmap.add str ajout sE, aE2)

let parcoursFonction fE sE (str, pL, pjT, _) = 
  let tL = List.fold_right 
    (fun (Param (str,t)) l -> if exists t sE then t::l else error ("undefined type : "^typeName t^" in function "^str))
    pL
    []
  in if exists pjT sE then Tmap.add str (tL, pjT) fE else error "type overload"

let rec parcoursExpr vE fE aE sE = function
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
  | ElvalueAffect (Lident str,e) -> Tmap.add str Any (parcoursExpr vE fE aE sE e)
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
  | Ewhile (e,b) -> let _ = parcoursBloc (parcoursExpr vE fE aE sE e) fE aE sE b in vE
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
  |Dstruct (Struct (a1,a2,a3))::tl ->
      let s,a = parcoursStruct sEnv aEnv (a1,a2,a3) in
      parcours1 vEnv fEnv s a tl
  |Dfonction (Function (a,b,c,d))::tl -> 
      let fEnv2 = parcoursFonction fEnv sEnv (a,b,c,d) in
      parcours1 vEnv fEnv2 sEnv aEnv tl
  |Dexpr e::tl ->
      let vEnv2 = parcoursExpr vEnv fEnv sEnv aEnv e in
      parcours1 vEnv2 fEnv sEnv aEnv tl

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
  | Eapplication (_,ident,eL) -> 
    if ident = "print" || ident = "println" then Nothing else
    if Tmap.mem ident fE then 
      let (tL,t) = Tmap.find ident fE in
      let rec aux l1 l2 = match l1,l2 with
        |[],[] -> true
        |t::tl1, e::tl2 -> (compatible t (testTypageE vE fE sE aE e)) && aux tl1 tl2
        | _, _ -> raise (Ast.Typing_Error)
      in if aux tL eL then t else raise (Ast.Typing_Error)
    else S ident
  | Enot e ->
    if compatible Bool (testTypageE vE fE sE aE e)
    then Bool
    else raise Ast.Typing_Error
  | Eminus e ->
    if compatible Int64 (testTypageE vE fE sE aE e)
    then Int64
    else raise Ast.Typing_Error
  | Ebinop (o,e1,e2) -> begin
    let t1 = testTypageE vE fE sE aE e1 in
    let t2 = testTypageE vE fE sE aE e2 in
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
      |Lident str -> Tmap.find str vE
      |Lindex (e,n) ->
        let (b,t2, nm) = Tmap.find n aE in
        let t3 = testTypageE vE fE sE aE e in
        if compatible t3 (S nm) then t2 else raise Ast.Typing_Error
    end
  | ElvalueAffect (lv,e) -> begin
    let t = testTypageE vE fE sE aE e in
    match lv with 
      | Lident str -> if compatible t (Tmap.find str vE) then t else raise Ast.Typing_Error
      | Lindex (e,n) -> 
        let (b,t2,nm) = Tmap.find n aE in
        if b then
          let t3 = testTypageE vE fE sE aE e in
          if compatible t3 (S nm) then if compatible t t2 then t
            else raise Ast.Typing_Error
          else raise Ast.Typing_Error
        else raise Ast.Typing_Error
    end
  | Ereturn _ -> raise Ast.Typing_Error
  | Efor (i,e1,e2,b) -> if compatible (testTypageE vE fE sE aE  e1) Int64 && compatible (testTypageE vE fE sE aE e2) Int64
    then Nothing
    else raise Ast.Typing_Error
  | Ewhile (e,b) ->
    if compatible Bool (testTypageE vE fE sE aE e)
    then Nothing
    else raise Ast.Typing_Error
  | Eif (e,b,els) ->
      if compatible Bool (testTypageE vE fE sE aE e)
      then Any
      else raise Ast.Typing_Error
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


let rec testTypageBlocF vE fE sE aE pjT = function
  |[] -> Nothing
  |None::tl -> testTypageBlocF vE fE sE aE pjT tl
  |[Some e] -> testTypBlocF2 vE fE sE aE pjT e
  |Some e::tl -> let _ = testTypBlocF2 vE fE sE aE pjT e in testTypageBlocF vE fE sE aE pjT tl
and testTypBlocF2 vE fE sE aE pjT = function
  | Eentier _ -> Int64
  | Echaine _ -> String
  | Etrue _ | Efalse _ -> Bool
  | EentierIdent (_,_,str) -> if compatible Int64 (Tmap.find str vE) then Int64 else raise Ast.Typing_Error
  | EentierParG (_,_,b1) -> if compatible Int64 (testTypFBloc1 vE fE sE aE pjT b1) then Int64 else raise Ast.Typing_Error
  | Ebloc1 b1 -> testTypFBloc1 vE fE sE aE pjT b1
  | EparDIdent (_,e,ident) -> if compatible Int64 (Tmap.find ident vE) then
        if compatible Int64 (testTypBlocF2 vE fE sE aE pjT e)
        then Int64
        else raise (Ast.Typing_Error)
      else raise (Ast.Typing_Error)
  | Eapplication (_,ident,eL) -> 
    if ident = "print" || ident = "println" then Nothing else
    if Tmap.mem ident fE then 
      let (tL,t) = Tmap.find ident fE in
      let rec aux l1 l2 = match l1,l2 with
        |[],[] -> true
        |t::tl1, e::tl2 -> (compatible t (testTypBlocF2 vE fE sE aE pjT e)) && aux tl1 tl2
        | _, _ -> raise (Ast.Typing_Error)
      in if aux tL eL then t else raise (Ast.Typing_Error)
    else S ident
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
      |Lident str -> Tmap.find str vE
      |Lindex (e,n) ->
        let (b,t2, nm) = Tmap.find n aE in
        let t3 = testTypBlocF2 vE fE sE aE pjT e in
        if compatible t3 (S nm) then t2 else raise Ast.Typing_Error
    end
  | ElvalueAffect (lv,e) -> begin
    let t = testTypBlocF2 vE fE sE aE pjT e in
    match lv with 
      | Lident i -> if compatible t (Tmap.find i vE) then t else raise Ast.Typing_Error
      | Lindex (e,n) -> 
        let (b,t2,nm) = Tmap.find n aE in
        if b then
          let t3 = testTypBlocF2 vE fE sE aE pjT e in
          if compatible t3 (S nm) then if compatible t t2 then t
            else raise Ast.Typing_Error
          else raise Ast.Typing_Error
        else raise Ast.Typing_Error
    end
  | Ereturn None -> if compatible pjT Nothing then Nothing else raise Ast.Typing_Error
  | Ereturn (Some e) -> if compatible pjT (testTypBlocF2 vE fE sE aE pjT e) then Nothing else raise Ast.Typing_Error
  | Efor (i,e1,e2,b) -> if compatible (testTypBlocF2 vE fE sE aE pjT e1) Int64 && compatible (testTypBlocF2 vE fE sE aE pjT e2) Int64
    then Nothing
    else raise Ast.Typing_Error
  | Ewhile (e,b) ->
    if compatible Bool (testTypBlocF2 vE fE sE aE pjT e)
    then Nothing
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

let testTypageF (vE:varEnv) (fE:funcEnv) (sE:structEnv) (aE:argsEnv) f =
  let Function (str,pL,pjT, Bloc b) = f in
  let vE1 = List.fold_right (fun (Param (str,t)) m -> if exists t sE then Tmap.add str t m else raise Ast.Typing_Error) pL vE in
  let vE2 = parcoursBloc vE1 fE aE sE (Bloc b) in
  let vE3 = List.fold_right (fun (Param (str,t)) m -> if exists t sE then Tmap.add str t m else raise Ast.Typing_Error) pL vE2 in
  let _ = testTypageBlocF vE3 fE sE aE pjT b in ()


let rec parcours2 (vEnv:varEnv) (fEnv:funcEnv) (sEnv:structEnv) (aEnv:argsEnv) = function
  |[] ->  ()
  |Dstruct _::tl -> parcours2 vEnv fEnv sEnv aEnv tl
  |Dfonction f::tl -> let () = testTypageF vEnv fEnv sEnv aEnv f in parcours2 vEnv fEnv sEnv aEnv tl
  |Dexpr e::tl -> let _ = testTypageE vEnv fEnv sEnv aEnv e in parcours2 vEnv fEnv sEnv aEnv tl


let verificationType declL =
    let DeclarationList dl = declL in
    let vE, fE,sE,aE = parcours1 emptyVenv emptyFenv emptySenv emptyAenv dl in
    parcours2 vE fE sE aE dl

