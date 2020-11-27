open Lexer
open Parser
open Ast
open Astype
open Typer
open Hyper
open Utilities
open Astinterp

let print_limit = 4;;

let rec len liste =
  match liste with
  | [] -> 0
  | _ :: q -> 1 + (len q)
;;

let rec appartient elt liste =
  match liste with
  | [] -> false
  | t :: q -> t = elt || appartient elt q
;;

let rec print_value nombre = function
  | Vnothing -> "Nothing"
  | Vbool true -> "true"
  | Vbool false -> "false"
  | Vint n -> string_of_int n
  | Vstring s -> s
  | Vfloat f -> string_of_float f
  | Vstruct s ->
    if nombre > print_limit then "..."
    else
    begin
      let (n, b, identlist, htbl) = s in
      let res = ref "" in
      if b then begin res := !res ^ "mutable " end;
      res := !res ^ n;
      res := !res ^ "{";
      let rec add_to_str point liste=
        match liste with
          | [] -> ()
          | [t] -> res := !res ^ (t ^ " : " ^ (print_value (nombre + 1) (Hashtbl.find htbl t)));
          | t :: q -> res := !res ^ (t ^ " : " ^ (print_value (nombre + 1) (Hashtbl.find htbl t)) ^ "; "); add_to_str point q
      in
      add_to_str res identlist;
      res := !res ^ "}";
      !res
    end
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
  | Param (_, _, _, Any) :: q1, _ :: q2 -> compatible q1 q2
  | Param (_, _, _, Nothing) :: q, _ -> false
  | Param (_, _, _, Int64) :: q1, Vint _ :: q2 -> compatible q1 q2
  | Param (_, _, _, Bool) :: q1, Vbool _ :: q2 -> compatible q1 q2
  | Param (_, _, _, String) :: q1, Vstring _ :: q2 -> compatible q1 q2
  | Param (_, _, _, S s1) :: q1, Vstruct s2 :: q2 ->
    let (i, _, _, _) = s2 in (s1 = i) && (compatible q1 q2)
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
    let (i, _, _, _) = s2 in (s1 = i)
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
  | Dstruct (b, _, i, params) ->
    let sn = (b, i, params) in(* Nouvelle structure *)
    sI := Imap.add i sn !sI;
  | _ -> ()
;;

let construct_function f fI sI =
  match f with
  | Dfonction (_, i, params, _, ty, bloc, _) ->
    if Imap.mem i !fI then
      begin
        let l = Imap.find i !fI in
        fI := Imap.add i ((i, params, ty, bloc) :: l) !fI;
      end
    else
      fI := Imap.add i [(i, params, ty, bloc)] !fI;
  | _ -> ()
;;

let rec construct_declaration_list l fI sI =
  match l with
  | [] -> ()
  | Dstruct _ as d :: q -> construct_struct d fI sI; construct_declaration_list q fI sI
  | Dfonction _ as f :: q -> construct_function f fI sI; construct_declaration_list q fI sI
  | _ :: q -> construct_declaration_list q fI sI
;;


(* Fonction principale de construction de fonctions & structures *)
let construct code fI sI =
  match code with
    | DeclarationList l -> construct_declaration_list l fI sI
;;



(* Interpretation *)

let rec interp_expression e vI fI sI =
  let ep =
    match e with (pP, ep) -> ep
  in
  match ep with
  | Eentier i -> Vint i
  | Echaine s -> Vstring s
  | Etrue -> Vbool true
  | Efalse -> Vbool false
  | EentierIdent (p, i, s) -> interp_expression (p, Ebinop (p, Times, (p, Eentier i), (p, Elvalue (Lident (p, s))))) vI fI sI
  | EentierParG (p, i, b1) -> interp_expression (p, Ebinop (p, Times, (p, Eentier i), (p, Ebloc1 b1))) vI fI sI
  | Ebloc1 (p, eL) -> interp_expression_list_one eL vI fI sI
  | EparDIdent (e, p, i) -> interp_expression (p, Ebinop (p, Times, e, (p, Elvalue (Lident (p, i))))) vI fI sI
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
      | (Param (_, i, _, _)) :: q1, a :: q2 -> vI := Imap.add i a !vI; add_arguments q1 q2 vI
      | _ -> failwith "Error wtf"
    in
    let rec add_elements_to_struct identList values htbl =
      match identList, values with
      | [], [] -> ()
      | t1 :: q1, t2 :: q2 -> Hashtbl.add htbl t1 t2; add_elements_to_struct q1 q2 htbl
      | _, _ -> failwith "Incorrect number of arguments for struct" (* À améliorer *)
    in
    let rec print_function l acc =
      match l with
      | [] -> Printf.printf "%s" acc; Vnothing
      | t :: q -> print_function q (acc ^ (print_value 0 t))
    in
    if i = "print" then
        print_function expr ""
    else
      begin
        if i = "println"
          then print_function (expr @ [Vstring "\n"]) ""
        else
          begin
            if i = "div"
              then
                begin
                  match expr with
                  | [Vint n1; Vint n2] -> Vint (n1 / n2)
                  | _ -> failwith "Wrong arguments for div" (* À améliorer *)
                end
            else
              begin
                if Imap.mem i !fI then (* Essaye d'appliquer une fonction *)
                  begin
                    let (i, params, t, (p, b)) = parcours_fonctions (Imap.find i !fI) expr in
                    let vIp = ref !vI in
                    let () = add_arguments params expr vIp in
                    try
                      let liste = List.map (fun x -> Dexpr x) b in
                      let res = interp_declaration_list liste vIp fI sI false in
                      res
                    with Return vali ->
                      vali;
                  end
                else
                  begin
                    if Imap.mem i !sI then (* Essaye de construire une structure *)
                      begin
                        let (b, i, pList) = Imap.find i !sI in
                        let identList = List.map (fun (Param (a, i, b, e)) -> i) pList in
                        let htbl = Hashtbl.create (len pList) in
                        add_elements_to_struct identList expr htbl;
                        Vstruct (i, b, identList, htbl)
                      end
                    else
                      failwith ("Unknown function or structure " ^ i); (* À améliorer *)
                end
              end
          end
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
  | Ebinop (p, op, e1, e2) ->
    let e1p = interp_expression e1 vI fI sI in
    let e2p = interp_expression e2 vI fI sI in
    let vali =
      match op, e1p, e2p with
      | Plus, Vint i1, Vint i2 -> Vint (i1 + i2)
      | Minus, Vint i1, Vint i2 -> Vint (i1 - i2)
      | Times, Vint i1, Vint i2 -> Vint (i1 * i2)
      | Modulo, Vint i1, Vint i2 -> Vint (i1 mod i2)
      | Exp, Vint i1, Vint i2 -> Vint (puissance i1 i2)
      | Eq, _, _ ->
        let res =
        match e1p, e2p with
        | Vint i1, Vint i2 -> Vbool (i1 = i2)
        | Vbool i1, Vbool i2 -> Vbool (i1 = i2)
        | Vstring i1, Vstring i2 -> Vbool (i1 = i2)
        | _ -> failwith "Incompatible types" (* À améliorer *)
        in res
      | Neq, Vint i1, Vint i2 -> Vbool (i1 <> i2)
      | Lo, Vint i1, Vint i2 -> Vbool (i1 < i2)
      | Gr, Vint i1, Vint i2 -> Vbool (i1 > i2)
      | Leq, Vint i1, Vint i2 -> Vbool (i1 <= i2)
      | Geq, Vint i1, Vint i2 -> Vbool (i1 >= i2)
      | And, Vbool b1, Vbool b2 -> Vbool (b1 && b2)
      | Or, Vbool b1, Vbool b2 -> Vbool (b1 || b2)
      | _ -> failwith "Erreur de typage dans un opérateur binaire" (* À améliorer *)
    in vali
  | Elvalue lval ->
    let res =
      match lval with
      | Lident (p, i) -> Imap.find i !vI
      | Lindex (e, p, i) ->
        let vali = interp_expression e vI fI sI in
        match vali with
        | Vstruct (i0, b0, pList0, htbl0) -> Hashtbl.find htbl0 i
        | _ -> failwith "Value not a structure" (* À améliorer *)
   in res
  | ElvalueAffect (p, lval, e) ->
    let ep = interp_expression e vI fI sI in
    let () =
    match lval with
    | Lident (p, i) -> vI := Imap.add i ep !vI
    | Lindex (e, p, i) ->
      begin
      let res = interp_expression e vI fI sI in
      match res with
      | Vstruct (i0, true, pList0, htbl0) when appartient i pList0 -> Hashtbl.add htbl0 i ep;
      | _ -> print_endline "lol"; failwith "Structure non mutable ou alors pas de champ correspondant"; (* *)
      end
    in ep
  | Ereturn (p, e) ->
    let res =
      match e with
      | Some ep -> raise (Return (interp_expression ep vI fI sI))
      | None -> raise (Return Vnothing)
    in res
  | Efor (id, e1, e2, (p, b)) ->
    let v1, v2 = interp_expression e1 vI fI sI, interp_expression e2 vI fI sI in
    let n1, n2 =
      match v1, v2 with
      | Vint t1, Vint t2 -> t1, t2
      | _ -> failwith "Expected integer values in for bounds" (* À améliorer *)
    in
    let vIp = ref !vI in
    let liste = List.map (fun x -> Dexpr x) b in
    for i = n1 to n2 do
      vIp := Imap.add id (Vint i) !vIp;
      let _ = interp_declaration_list liste vIp fI sI false in ();
    done;
    Vnothing
  | Ewhile (e, (p, b)) ->
    let extract_bool = function
      | Vbool b -> b
      | _ -> failwith "Expected a boolean as while condition" (* À améliorer *)
    in
    let liste = List.map (fun x -> Dexpr x) b in
    while (extract_bool (interp_expression e vI fI sI)) do
      let _ = (interp_declaration_list liste vI fI sI false) in ();
    done;
    Vnothing
  | Eif (exp, b, els) -> Vint 0
and interp_expression_list_one liste vI fI sI=
  match liste with
  | [] -> failwith "Error empty bloc1" (* À améliorer *)
  | [e] -> interp_expression e vI fI sI
  | e :: q -> let _ = interp_expression e vI fI sI in
    interp_expression_list_one q vI fI sI
and interp_declaration_list l vI fI sI p =
  match l with
  | [] -> Vnothing
  | [Dexpr e] ->
    let res = interp_expression e vI fI sI in
    if res != Vnothing && p then print_string (print_value 0 res); res
  | Dexpr e :: q -> let _ = interp_expression e vI fI sI in (); interp_declaration_list q vI fI sI p;
  | _ :: q -> interp_declaration_list q vI fI sI p
;;

let interp_file file vI fI sI =
  construct file fI sI;
  match file with
  | DeclarationList l -> let _ = interp_declaration_list l vI fI sI true in ()
;;

(*
type varInterp = Astinterp.value Imap.t
type funcInterp = ((ident * (param list) * pjtype * bloc) list) Imap.t
type structInterp = (bool * string * (param list)) Imap.t
type argsInterp = (bool * Astype.pjtype * string) Imap.t
*)
