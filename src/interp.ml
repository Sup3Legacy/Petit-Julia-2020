open Lexer
open Parser
open Typer
open Ast
open Astype
open Hyper
open Utilities
open Astinterp
open Format

let rec print_value = function
  | Vnothing -> print_endline "Nothing"
  | Vbool true -> print_endline "True"
  | Vbool false -> print_endline "False"
  | Vint n -> print_endline (string_of_int n)
  | Vstring s -> print_endline s
  | Vfloat f -> print_endline (string_of_float f)
  | Vstruct s ->
    let (n, b, l) = !s in
    if b then print_string "mutable ";
    print_string n;
    print_string "{";
    let _ = List.iter (fun (s, v) -> print_string ("s : " ^ s);  print_value v; print_string ";") l in
    print_string "}";
;;

module Imap = Map.Make(String);; (* Map contenant les environnements typage *)

let globVenv = ref (Imap.empty : value Imap.t);;
let globFenv = ref (Imap.empty : ((Ast.ident * (Ast.param list) * Astype.pjtype * Ast.bloc) list) Imap.t);;
let globSenv = ref (Imap.empty : (bool * Ast.ident * (Ast.param list)) Imap.t);;

exception Return of value;;

let rec filter_option l =
  match l with
  | [] -> []
  | Some t :: q -> t :: (filter_option q)
  | _ :: q -> filter_option q
;;

let rec puissance n m =
  if m = 0 then 1
  else n * (puissance n (m - 1))
;;

let rec compatible pList expr =
  match pList, expr with
  | [], [] -> true
  | [], _ | _, [] -> false
  | Param (_, Any) :: q1, _ :: q2 -> compatible q1 q2
  | Param (_, Nothing) :: q, _ -> false
  | Param (_, Int64) :: q1, Vint _ :: q2 -> compatible q1 q2
  | Param (_, Bool) :: q1, Vbool _ :: q2 -> compatible q1 q2
  | Param (_, String) :: q1, Vstring _ :: q2 -> compatible q1 q2
  | Param (_, S s1) :: q1, Vstruct s2 :: q2 ->
    let (i, _, _) = !s2 in (s1 = i) && (compatible q1 q2)
  | _, _ ->false
;;

let compatible_un t1 t2 =
  match t1, t2 with
  | Any, _-> true
  | Nothing, _ -> false
  | Int64, Vint _ -> true
  | Bool, Vbool _ -> true
  | String, Vstring _ -> true
  | S s1, Vstruct s2 ->
    let (i, _, _) = !s2 in (s1 = i)
  | _, _ ->false
;;

let rec bloc_to_expressionlist l =
  match l with
  | [] -> []
  | Some e :: q -> (Dexpr e) :: (bloc_to_expressionlist q)
  | None :: q -> bloc_to_expressionlist q
;;

let rec bloc_to_expressionlist_bis l =
  match l with
  | [] -> []
  | Some e :: q -> e :: (bloc_to_expressionlist_bis q)
  | None :: q -> bloc_to_expressionlist_bis q
;;


(* Construction des talbes de fonctions et structures *)
let construct_struct s fI sI =
  match s with
  | Struct (b, i, params) ->
    let sn = (b, i, filter_option params) in(* Nouvelle structure *)
    sI := Imap.add i sn !sI;
;;

let construct_function f fI sI =
  match f with
  | Function (i, params, ty, bloc) ->
    if Imap.mem i !fI then
      begin
        let l = Imap.find i !fI in
        fI := Imap.add i ((i, params, ty, bloc) :: l) !fI;
      end
    else
      fI := Imap.add i [(i, params, ty, bloc)] !fI;
;;

let rec construct_declaration_list l fI sI =
  match l with
  | [] -> ()
  | Dstruct d :: q -> construct_struct d fI sI; construct_declaration_list q fI sI
  | Dfonction f :: q -> construct_function f fI sI; construct_declaration_list q fI sI
  | _ :: q -> construct_declaration_list q fI sI
;;


(* Fonction principale de construction de fonctions & structures *)
let construct code fI sI =
  match code with
    | DeclarationList l -> construct_declaration_list l fI sI
;;



(* Interpretation *)

let rec interp_expression e vI fI sI =
  match e with
  | Eentier (p, i) -> Vint i
  | Echaine (p, s) -> Vstring s
  | Etrue p -> Vbool true
  | Efalse b -> Vbool false
  | EentierIdent (p, i, s) -> interp_expression (Ebinop (Times, Eentier (p, i), Elvalue (Hyper.posVide, Lident s))) vI fI sI
  | EentierParG (p, i, b1) -> interp_expression (Ebinop (Times, Eentier (p, i), Ebloc1 b1)) vI fI sI
  | Ebloc1 (Bloc1 (e, Some (Bloc b1))) -> interp_expression_list_one (e :: (bloc_to_expressionlist_bis b1)) vI fI sI
  | Ebloc1 (Bloc1 (e, None)) -> interp_expression_list_one [e] vI fI sI
  | EparDIdent (p, e, i) -> interp_expression (Ebinop (Times, e, Elvalue (p, Lident i))) vI fI sI
  | Eapplication (p, i, liste) ->
    (* if not Imap.mem i !fI then failwith ("Unknown function "^i); (* À améliorer *) *)
    let expr = List.map (fun x -> interp_expression x vI fI sI) liste in
    let rec parcours_fonctions l expr =
      match l with
      | [] -> failwith ("Bad arguments for function "^i) (* À améliorer *)
      | (_, pList, _, fBloc) as f :: q ->
        if compatible pList expr then f else parcours_fonctions q expr
    in
    (* let adequate_structure s expr =
      let (b, i, pList) = s in
      compatible pList expr
    in *)
    let rec add_arguments params expr vI =
      match params, expr with
      | [], _ -> ()
      | (Param (i, _)) :: q1, a :: q2 -> vI := Imap.add i a !vI; add_arguments q1 q2 vI
      | _ -> failwith "Error wtf"
    in
    let rec create_struct pList expr =
      match pList, expr with
      | [], [] -> []
      | Param (a, t) :: q1, t2 :: q2 when not (compatible_un t t2) -> failwith "Wrong type in structure" (* À améliorer *)
      | Param (a, _) :: q1, t2 :: q2 -> (a, t2) :: (create_struct q1 q2)
      | _ -> failwith "Wrong number of arguments when constructing structure" (* À améliorer *)
    in
    if Imap.mem i !fI then (* Essaye d'appliquer une fonction *)
      begin
        let (i, p, t, Bloc b) = parcours_fonctions (Imap.find i !fI) expr in
        let vIp = ref !vI in
        let () = add_arguments p expr vIp in
        let resultat = ref Vnothing in
        try
          let _ = interp_declaration_list (bloc_to_expressionlist b) vIp fI sI in
          !resultat
        with Return vali ->
          resultat := vali;
          !resultat
      end
    else
      begin
        if Imap.mem i !sI then (* Essaye de construire une structure *)
          begin
            let (b, i, pList) = Imap.find i !sI in
            Vstruct (ref (i, b, create_struct pList expr))
          end
        else
          failwith ("Unknown function or structure " ^ i); (* À améliorer *)
    end
  | Enot e ->
    let res = interp_expression e vI fI sI in
    let vali =
      match res with
      | Vbool b -> Vbool (not b)
      | _ -> failwith "Erreur de type" (* À améliorer *)
    in vali
  | Eminus e ->
    let res = interp_expression e vI fI sI in
    let vali =
      match res with
      | Vint i -> Vint (- i)
      | _ -> failwith "Erreur de type" (* À améliorer *)
    in vali
  | Ebinop (op, e1, e2) ->
    let e1p = interp_expression e1 vI fI sI in
    let e2p = interp_expression e2 vI fI sI in
    let vali =
      match op, e1p, e2p with
      | Plus, Vint i1, Vint i2 -> Vint (i1 + i2)
      | Minus, Vint i1, Vint i2 -> Vint (i1 - i2)
      | Times, Vint i1, Vint i2 -> Vint (i1 * i2)
      | Modulo, Vint i1, Vint i2 -> Vint (i1 mod i2)
      | Exp, Vint i1, Vint i2 -> Vint (puissance i1 i2)
      | Eq, Vint i1, Vint i2 -> Vbool (i1 = i2)
      | Neq, Vint i1, Vint i2 -> Vbool (i1 <> i2)
      | Lo, Vint i1, Vint i2 -> Vbool (i1 < i2)
      | Gr, Vint i1, Vint i2 -> Vbool (i1 > i2)
      | Leq, Vint i1, Vint i2 -> Vbool (i1 <= i2)
      | Geq, Vint i1, Vint i2 -> Vbool (i1 >= i2)
      | And, Vbool b1, Vbool b2 -> Vbool (b1 && b2)
      | Or, Vbool b1, Vbool b2 -> Vbool (b1 || b2)
      | _ -> failwith "Erreur de typage dans un opérateur binaire" (* À améliorer *)
    in vali
  | Elvalue (p, lvalue) ->
    let res =
      match lvalue with
      | Lident i -> Imap.find i !vI
      | Lindex (i, s) -> Vint 0
   in res
  | ElvalueAffect (lval, e) ->
    let ep = interp_expression e vI fI sI in
    let () =
    match lval with
    | Lident i -> vI := Imap.add i ep !vI
    | Lindex (i, s) ->
      let res =
      match i with
      | Elvalue (p, elval) -> ()
      | _ -> ()
      in res
    in Vnothing
  | Ereturn e ->
    let res =
      match e with
      | Some ep -> raise (Return (interp_expression ep vI fI sI))
      | None -> raise (Return Vnothing)
    in res
  | Efor (id, e1, e2, Bloc b) ->
    let v1, v2 = interp_expression e1 vI fI sI, interp_expression e2 vI fI sI in
    let n1, n2 =
      match v1, v2 with
      | Vint t1, Vint t2 -> t1, t2
      | _ -> failwith "Expected integer values in for bounds" (* À améliorer *)
    in
    let vIp = ref !vI in
    let liste = bloc_to_expressionlist b in
    for i = n1 to n2 do
      vIp := Imap.add id (Vint i) !vIp;
      interp_declaration_list liste vIp fI sI;
    done;
    Vnothing
  | Ewhile (e, Bloc b) ->
    let extract_bool = function
      | Vbool b -> b
      | _ -> failwith "Expected a boolean as while condition" (* À améliorer *)
    in
    let liste = bloc_to_expressionlist b in
    while (extract_bool (interp_expression e vI fI sI)) do
      let _ = (interp_declaration_list liste vI fI sI) in ();
    done;
    Vnothing
  | Eif (exp, b, els) -> Vint 0
and interp_expression_list_one liste vI fI sI=
  match liste with
  | [] -> failwith "Error empty bloc1" (* À améliorer *)
  | [e] -> interp_expression e vI fI sI
  | e :: q -> let _ = interp_expression e vI fI sI in
    interp_expression_list_one q vI fI sI
and interp_declaration_list l vI fI sI =
  match l with
  | [] -> ()
  | [Dexpr e] ->
    let res = interp_expression e vI fI sI in
    if res <> Vnothing then print_value res;
  | Dexpr e :: q -> interp_expression e vI fI sI; interp_declaration_list q vI fI sI;
  | _ :: q -> interp_declaration_list q vI fI sI
;;

let interp_file file vI fI sI =
  construct file fI sI;
  match file with
  | DeclarationList l -> interp_declaration_list l vI fI sI
;;

(*
type varInterp = Astinterp.value Imap.t
type funcInterp = ((ident * (param list) * pjtype * bloc) list) Imap.t
type structInterp = (bool * string * (param list)) Imap.t
type argsInterp = (bool * Astype.pjtype * string) Imap.t
*)
