(* Gère les dépendances des fichiers. *)

open Ast
open Astype

module Import_packages_set = Set.Make(String) (* Ensemble des packages déjà importés; évite les boucles d'importation *)

let currentPackages = ref Import_packages_set.empty;;

let get_package identifiant =
  let file = open_in (PPkg.get_package_path identifiant) in (* À améliorer *)
  let lb = Lexing.from_channel file in
  let e = Parser.fichier Lexer.token lb in
  match e with DeclarationList l -> l
;;

let add_prefix_ident ident name =
  match ident with 
  | "println" | "print" | "newarray" | "_setelement" | "_getelement" | "div" | "array_length" | "input_int" -> ident
  | _ -> name ^ ident
;;

let rec add_prefix_expr e name =
  let (p, e1) = e in 
  let e2 =  
  (match e1 with
  | Eentier _ as s -> s
  | Eflottant _ as s -> s
  | Echaine _ as s -> s
  | Etrue as s -> s
  | Efalse as s -> s
  | EentierIdent (p, i, ide) -> EentierIdent (p, i, add_prefix_ident ide name)
  | EentierParG (p, i, bloc) -> EentierParG (p, i, add_prefix_bloc bloc name)
  | Ebloc1 b -> Ebloc1 (add_prefix_bloc b name)
  | EparDIdent (exp, pos, id) -> EparDIdent (add_prefix_expr exp name, pos, add_prefix_ident id name)
  | Eapplication (p, id, expList) -> Eapplication (p, add_prefix_ident id name, List.map (fun x -> add_prefix_expr x name) expList)
  | Enot e -> Enot (add_prefix_expr e name)
  | Eminus e -> Eminus (add_prefix_expr e name)
  | Ebinop (p, o, e1, e2) -> Ebinop (p, o, add_prefix_expr e1 name, add_prefix_expr e2 name)
  | Elvalue lval -> Elvalue (add_prefix_lvalue lval name)
  | ElvalueAffect (p, lval, exp) -> ElvalueAffect (p, add_prefix_lvalue lval name, add_prefix_expr exp name)
  | Ereturn (pos, None) as s -> s
  | Ereturn (pos, Some e) -> Ereturn (pos, Some (add_prefix_expr e name))
  | Efor (id, e1, e2, b) -> Efor (add_prefix_ident id name, add_prefix_expr e1 name, add_prefix_expr e2 name, add_prefix_bloc b name)
  | Ewhile (e, b) -> Ewhile (add_prefix_expr e name, add_prefix_bloc b name)
  | Eif (e, b, e_) -> Eif (add_prefix_expr e name, add_prefix_bloc b name, add_prefix_else_ e_ name)
  )
  in (p, e2)
and add_prefix_lvalue lval name =
  match lval with
  | Lident (p, id) -> Lident (p, add_prefix_ident id name)
  | Lindex (e, p, id) -> Lindex (add_prefix_expr e name, p, add_prefix_ident id name)
  | Larray (e1, e2) -> Larray (add_prefix_expr e1 name, add_prefix_expr e2 name)
[@@deriving show]
and add_prefix_else_ else_ name =
  match else_ with
  | Iend -> Iend
  | Ielse b -> Ielse (add_prefix_bloc b name)
  | Ielseif (e, b, e_) -> Ielseif (add_prefix_expr e name, add_prefix_bloc b name, add_prefix_else_ e_ name)
and add_prefix_bloc b name =
  match b with 
  | (pos, liste) -> (pos, List.map (fun x -> add_prefix_expr x name) liste)
;;

let add_prefix_param param name =
  match param with
  | Param (pos1, id, pos2, S id_) ->
    Param (pos1, add_prefix_ident id name, pos2, S (add_prefix_ident id_ name))
    | Param (pos1, id, pos2, type_) ->
      Param (pos1, add_prefix_ident id name, pos2, type_)
;;

let add_prefix_fonction fonc name =
  match fonc with
  | Dfonction (pos1, id, paramList, pos2, type_, bloc, docstring) ->
    Dfonction (pos1, add_prefix_ident id name, List.map (fun x -> add_prefix_param x name) paramList, pos2, type_, add_prefix_bloc bloc name, docstring)
  | _ as s -> s (* Juste pour avoir la paix avec le détecteur de pattern non complet *)
;;

let add_prefix_struct struct_ name =
  match struct_ with
  | Dstruct (boo, pos, id, paramList) ->
    Dstruct (boo, pos, add_prefix_ident id name, List.map (fun x -> add_prefix_param x name) paramList)
  | _ as s -> s (* Juste pour avoir la paix avec le détecteur de pattern non complet *)
;;

let rec add_prefix decl_list name =
  match decl_list with 
  | Dexpr s :: q -> (Dexpr (add_prefix_expr s name)):: (add_prefix q name)
  | Dstruct _ as s :: q -> (add_prefix_struct s name) :: (add_prefix q name)
  | Dfonction _ as s :: q -> (add_prefix_fonction s name) :: (add_prefix q name)
  | [] -> []
;;

let rec handle_dep decl_list =
  match decl_list with
  | Dexpr(pos1, Eapplication (pos2, ident, [(pos3, Echaine pack_name)])) :: q when ident = "include"->
    ((if false && Import_packages_set.mem pack_name !currentPackages then (* Cela n'a plus lieu d'être, avec le système de namespace *)
        begin (* Si le package est déjà importé *)
          print_string ("Already imported package : " ^ pack_name);
          []
        end
        else
          begin
            let ajout =
              try
                begin
                  let a = handle_dep (get_package pack_name) in
                  currentPackages := Import_packages_set.add pack_name !currentPackages;
                  print_endline ("Successfully imported package : " ^ pack_name);
                  let n = String.length pack_name in
                  add_prefix a ((String.sub pack_name 0 (n -  3)) ^ "::")
                end
              with a ->
                raise a
                begin
                  print_endline ("Unable to import package : " ^ pack_name);
                  []
                end
            in ajout
          end) @ (handle_dep q))
  | Dexpr _ as t :: q -> t :: (handle_dep q)
  | Dstruct _ as t :: q -> t :: q
  | Dfonction _ as t :: q -> t :: q
  | [] -> []
;;

let add_dependencies parsed_file =
  let liste = match parsed_file with DeclarationList l -> l in
  DeclarationList (handle_dep liste)
;;

let get_parsed_file lb =
  let e = Parser.fichier Lexer.token lb in
  add_dependencies e
;;
