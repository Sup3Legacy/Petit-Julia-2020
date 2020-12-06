(*
###########################################################
#                                                         #
#                       Interpréteur                      #
#                                                         #
#     Il donne les fonctions permettant de traiter et     #
#                  interpréter du code.                   #
#                                                         #
###########################################################
*)

open Lexer
open Parser
open Ast
open Astype
open Typer
open Hyper
open Utilities
open Astinterp

let interp_error s = raise (Ast.Interp_Error_Msg s);;
exception Return of value;; (* exception qui sert aux returns *)

(* Fonctions usuelles *)
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

(* fonctions pour la pile d'affichage *)
let positionDansPile2 element pile =
  let rec aux n = function
    |[] -> None
    |hd::tl when hd==element -> Some n
    | _::tl -> aux (n+1) tl
  in aux 1 pile

let positionDansPile element pile = match  element with
| Vstruct s -> positionDansPile2 s pile
| _ -> None

(* Fonction d'affichage de valeur *)
let rec print_value pile = function
  | Vnothing -> "Nothing"
  | Vbool true -> "true"
  | Vbool false -> "false"
  | Vint n -> Int64.to_string n
  | Vstring s -> s
  | Vfloat f -> string_of_float f
  | Vstruct s -> (* Le cas d'une structure est délicat. Si on a une référence circulaire, il faut l'identifier *)
    begin
      let (n, b, identlist, htbl) = s in
      let res = ref "" in
      if b then begin res := !res ^ "mutable " end;
      res := !res ^ n;
      res := !res ^ "{";
      let pile = s::pile in
      let rec add_to_str point liste=
        match liste with
          | [] -> ()
          | [t] -> begin
            let valeur = (Hashtbl.find htbl t) in
            match positionDansPile valeur pile with
            |None -> res := !res ^ (t ^ " : " ^ (print_value pile valeur))
            |Some i -> res := !res ^ (t ^ " : #= circular reference @-"^string_of_int i^" =#")
            end
          | t :: q -> begin let valeur = (Hashtbl.find htbl t) in
            match positionDansPile valeur pile with
              |None -> res := !res ^ (t ^ " : " ^ (print_value pile valeur) ^ "; "); add_to_str point q
              |Some i -> res := !res ^ (t ^ " : #= circular reference @-"^string_of_int i^" =#; ");
            add_to_str point q
            end
      in
      add_to_str res identlist;
      res := !res ^ "}";
      !res
    end
;;

(* Environnements : variables, fonctions et structures *)
let globVenv = ref (Imap.singleton "nothing" Vnothing : Astinterp.varEnv);;
let globFenv = ref (Imap.empty : Astinterp.funcEnv);;
let globSenv = ref (Imap.empty : Astinterp.structEnv);;

let rec filter_option l =
  match l with
  | [] -> []
  | Some t :: q -> t :: (filter_option q)
  | _ :: q -> filter_option q
;;

let int2 = Int64.of_int 2;;
let rec puissance n m =
  if Int64.equal m Int64.zero then n
  else let rem = Int64.rem m int2 in
    if Int64.equal rem Int64.zero then (puissance (Int64.mul n n) (Int64.div m int2))
    else Int64.mul n (puissance (Int64.mul n n ) (Int64.div m int2))
;;

(* Pour tester la compatibilité des arguments d'un appel avec les types attendus *)
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

(* Construction des tables de fonctions et structures *)
let construct_struct s fI sI =
  match s with
  | Dstruct (b, _, i, params) ->
    let sn = (b, i, params) in(* Nouvelle structure *)
    sI := Imap.add i sn !sI;
  | _ -> ()
;;

(* Construction d'une fonction dans l'environnement *)
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



(* Interprétation *)
let rec interp_expression e vI fI sI =
  let ep =
    match e with (pP, ep) -> ep
  in
  match ep with
  | Eentier i -> Vint i
  | Eflottant f -> Vfloat f
  | Echaine s -> Vstring s
  | Etrue -> Vbool true
  | Efalse -> Vbool false
  | EentierIdent (p, i, s) -> interp_expression (p, Ebinop (p, Times, (p, Eentier i), (p, Elvalue (Lident (p, s))))) vI fI sI
  | EentierParG (p, i, b1) -> interp_expression (p, Ebinop (p, Times, (p, Eentier i), (p, Ebloc1 b1))) vI fI sI
  | Ebloc1 (p, eL) -> interp_expression_list_one eL vI fI sI
  | EparDIdent (e, p, i) -> interp_expression (p, Ebinop (p, Times, e, (p, Elvalue (Lident (p, i))))) vI fI sI
  | Eapplication (p, i, liste) ->
    let expr = List.map (fun x -> interp_expression x vI fI sI) liste in
    let rec parcours_fonctions l expr = (* Recherche la fonction compatible avec l'arument *)
      match l with
      | [] -> interp_error ("Bad arguments for function "^i)
      | (_, pList, _, fBloc) as f :: q ->
        if compatible pList expr then f else parcours_fonctions q expr
    in
    let rec add_arguments params expr vI =
      (* Ajoute les arguments à l'environnement local de la fonction *)
      match params, expr with
      | [], _ -> ()
      | (Param (_, i, _, _)) :: q1, a :: q2 -> vI := Imap.add i a !vI; add_arguments q1 q2 vI
      | _ -> interp_error "Error wtf"
    in
    let rec add_elements_to_struct identList values htbl =
      (* Ajoute à la table de hachage des champs de la structure leur valeur *)
      match identList, values with
      | [], [] -> ()
      | t1 :: q1, t2 :: q2 -> Hashtbl.add htbl t1 t2; add_elements_to_struct q1 q2 htbl
      | _, _ -> interp_error "Incorrect number of arguments for struct"
    in
    let rec print_function l acc =
      (* fonction générique print *)
      match l with
      | [] -> Printf.printf "%s" acc; Vnothing
      | t :: q -> print_function q (acc ^ (print_value [] t))
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
                  | [Vint n1; Vint n2] -> Vint (Int64.div n1 n2)
                  | _ -> interp_error "Wrong arguments for div"
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
                      interp_error ("Unknown function or structure " ^ i);
                end
              end
          end
        end
  | Enot e ->
    let res = interp_expression e vI fI sI in
    let vali =
      match res with
      | Vbool b -> Vbool (not b)
      | _ -> interp_error "Erreur de type pour l'opérateur !"
    in vali
  | Eminus e ->
    let res = interp_expression e vI fI sI in
    let vali =
      match res with
      | Vint i -> Vint (Int64.neg i)
      | _ -> interp_error "Erreur de type pour l'opérateur -"
    in vali
  | Ebinop (p, op, e1, e2) ->
    let e1p = interp_expression e1 vI fI sI in
    let e2p = interp_expression e2 vI fI sI in
    let convert_to_float = function
      | Vfloat f -> f
      | Vint i -> Int64.to_float i
      | _ -> failwith "Expected integer of float" (* À améliorer *)
    in
    let vali =
      match op, e1p, e2p with
      | Plus, Vint i1, Vint i2 -> Vint (Int64.add i1 i2)
      | Minus, Vint i1, Vint i2 -> Vint (Int64.sub i1 i2)
      | Times, Vint i1, Vint i2 -> Vint (Int64.mul i1 i2)
      | Modulo, Vint i1, Vint i2 -> Vint (Int64.rem i1 i2)
      | Exp, Vint i1, Vint i2 -> Vint (puissance i1 i2)

      | Plus, Vfloat f1, n2 -> Vfloat (f1 +. (convert_to_float n2))
      | Minus, Vfloat f1, n2 -> Vfloat (f1 -. (convert_to_float n2))
      | Times, Vfloat f1, n2 -> Vfloat (f1 *. (convert_to_float n2))
      | Exp, Vfloat f1, n2 -> Vfloat (f1 ** (convert_to_float n2))

      | Plus, n1, Vfloat f2 -> Vfloat ((convert_to_float n1) +. f2)
      | Minus, n1, Vfloat f2 -> Vfloat ((convert_to_float n1) -. f2)
      | Times, n1, Vfloat f2 -> Vfloat ((convert_to_float n1) *. f2)
      | Exp, n1, Vfloat f2 -> Vfloat ((convert_to_float n1) ** f2)

      | Eq, _, _ ->
        let res =
        match e1p, e2p with
        | Vint i1, Vint i2 -> Vbool (i1 = i2)
        | Vbool i1, Vbool i2 -> Vbool (i1 = i2)
        | Vstring i1, Vstring i2 -> Vbool (i1 = i2)
        | Vstruct (str1,_,_,l1), Vstruct (str2,_,_,l2) -> Vbool (str1==str2 && l1 == l2)
        | Vnothing, Vnothing -> Vbool true
        | _ -> Vbool false
        in res
      | Neq, _, _ ->
        let res =
        match e1p, e2p with
        | Vint i1, Vint i2 -> Vbool (i1 <> i2)
        | Vbool i1, Vbool i2 -> Vbool (i1 <> i2)
        | Vstring i1, Vstring i2 -> Vbool (i1 <> i2)
        | Vstruct (str1,_,_,l1), Vstruct (str2,_,_,l2) -> Vbool (str1<>str2 || not (l1 == l2))
        | Vnothing, Vnothing -> Vbool false
        | _ -> Vbool true
        in res
      | Lo, Vint i1, Vint i2 -> Vbool (i1 < i2)
      | Gr, Vint i1, Vint i2 -> Vbool (i1 > i2)
      | Leq, Vint i1, Vint i2 -> Vbool (i1 <= i2)
      | Geq, Vint i1, Vint i2 -> Vbool (i1 >= i2)
      | And, Vbool b1, Vbool b2 -> Vbool (b1 && b2)
      | Or, Vbool b1, Vbool b2 -> Vbool (b1 || b2)
      | _ -> interp_error "Erreur de typage dans un opérateur binaire" (* À améliorer *)
    in vali
  | Elvalue lval ->
    let res =
      match lval with
      | Lident (p, i) -> Imap.find i !vI
      | Lindex (e, p, i) ->
        let vali = interp_expression e vI fI sI in
        match vali with
        | Vstruct (i0, b0, pList0, htbl0) -> Hashtbl.find htbl0 i
        | _ -> interp_error "Value not a structure, couldn't get field" (* À améliorer *)
   in res
  | ElvalueAffect (p, lval, e) ->
    let ep = interp_expression e vI fI sI in
    let () =
    match lval with
    | Lident (p, i) -> vI := Imap.add i ep !vI
    | Lindex (e, p, i) -> (* Si on affecte le champ d'une structure *)
      begin
      let res = interp_expression e vI fI sI in
      match res with
      | Vstruct (i0, muta, pList0, htbl0) when muta && (appartient i pList0) -> Hashtbl.add htbl0 i ep;
      | _ -> interp_error "Structure non mutable ou alors pas de champ correspondant"; (* *)
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
      | _ -> interp_error "Expected integer values in for bounds"
    in
    let liste = List.map (fun x -> Dexpr x) b in
    let i = ref n1 in
    while Int64.compare !i n2 < 0 do
      vI := Imap.add id (Vint !i) !vI;
      let _ = interp_declaration_list liste vI fI sI false in
      i := Int64.succ !i
    done;
    Vnothing
  | Ewhile (e, (p, b)) ->
    let extract_bool = function
      | Vbool b -> b
      | _ -> interp_error "Expected a boolean as while condition"
    in
    let liste = List.map (fun x -> Dexpr x) b in
    while (extract_bool (interp_expression e vI fI sI)) do
      let _ = (interp_declaration_list liste vI fI sI false) in ();
    done;
    Vnothing
  | Eif (exp, (p, expList), els) ->
    match (interp_expression exp vI fI sI) with
    | Vbool true -> interp_expression_list_one expList vI fI sI
    | Vbool false -> interp_else els vI fI sI
    | _ -> interp_error "Need a bool in condition of if statement"
and interp_else els vI fI sI =
    match els with
    | Iend -> Vnothing
    | Ielse (p, expList) -> interp_expression_list_one expList vI fI sI
    | Ielseif ((p, exp), b, els) -> interp_expression (p, Eif ((p, exp), b, els)) vI fI sI
and interp_expression_list_one liste vI fI sI=
  match liste with
  | [] -> interp_error "Error empty bloc1"
  | [e] -> interp_expression e vI fI sI
  | e :: q -> let _ = interp_expression e vI fI sI in
    interp_expression_list_one q vI fI sI
and interp_declaration_list l vI fI sI p =
  match l with
  | [] -> Vnothing
  | [Dexpr e] ->
    let res = interp_expression e vI fI sI in
    if res != Vnothing && p then
    begin
      let v = print_value [] res in
      print_string v;
      if v = "69" then print_endline "\nNice.";
      if v = "42" then print_endline "\nNoice.";
    end;
    res
  | Dexpr e :: q -> let _ = interp_expression e vI fI sI in (); interp_declaration_list q vI fI sI p;
  | _ :: q -> interp_declaration_list q vI fI sI p
;;

let interp_file file =
  construct file globFenv globSenv;
  match file with
  | DeclarationList l -> let _ = interp_declaration_list l globVenv globFenv globSenv true in ()
;;

let flush () =
  globVenv := Imap.singleton "nothing" Vnothing;
  globFenv := Imap.empty;
  globSenv := Imap.empty
;;
