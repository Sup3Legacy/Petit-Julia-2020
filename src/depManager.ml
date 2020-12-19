(* Gère les dépendances des fichiers. *)

open Ast
open Astype

module Import_packages_set = Set.Make(String) (* Ensemble des packages déjà importés; évite les boucles d'importation *)

let currentPackages = ref Import_packages_set.empty;;

let get_package identifiant =
  let file = open_in identifiant in (* À améliorer *)
  let lb = Lexing.from_channel file in
  let e = Parser.fichier Lexer.token lb in
  match e with DeclarationList l -> l
;;

let rec handle_dep decl_list =
  match decl_list with
  | Dexpr(pos1, Eapplication (pos2, ident, [(pos3, Echaine pack_name)])) :: q when ident = "include"->
    ((if Import_packages_set.mem pack_name !currentPackages then
        begin (* Si le package est déjà importé *)
          print_string ("Already imported package : " ^ pack_name);
          []
        end
        else
          begin
            let ajout =
              try
                begin
                  let a = get_package pack_name in
                  currentPackages := Import_packages_set.add pack_name !currentPackages;
                  print_endline ("Successfully imported package : " ^ pack_name);
                  a
                end
              with _ ->
                begin
                  print_endline ("Unable to import package : " ^ pack_name);
                  []
                end
            in ajout
          end) @ (handle_dep q))
  | Dexpr _ as t :: q -> t :: (handle_dep q)
  | Dstruct _ :: q -> q
  | Dfonction _ :: q -> q
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
