(*
###########################################################
#                                                         #
#                  Fonctions utilitaires                  #
#   Il s'agit de fonctions annexes plus ou moins utiles.  #
#                                                         #
###########################################################
*)

open Ast

let rec clean_option_list liste =
  match liste with
  | [] -> []
  | None :: q -> clean_option_list q
  | t :: q -> t :: (clean_option_list q)
;;

(*
Fonction de nettoyage de l'arbre syntaxique généré par le parsing. Elle éliminait notamment les listes d'options.
Ce problème ayant disparu avec la refonte du parser, elle ne sert pour l'instant à rien
*)

(*
let rec clean_file x =
  match x with
  | DeclarationList d -> DeclarationList (clean_decllist d)
and clean_decllist x =
  match x with
  | [] -> []
  | t :: q -> (clean_decl t) :: (clean_decllist q)
and clean_decl x =
  match x with
  | Dstruct s -> Dstruct (clean_struct s)
  | Dfonction f -> Dfonction (clean_function f)
  | Dexpr e -> Dexpr (clean_expr e)
and clean_struct x =
  match x with
  | Struct (b, i, liste) -> Struct (b, i, clean_option_list liste)
and clean_function x =
  match x with
  | Function (id, args, typ, bloc) -> Function (id, args, typ, clean_bloc bloc)
and clean_expr x =
  match x with
  | Ebloc1 b -> Ebloc1 (clean_bloc1 b)
  | Efor (i, e1, e2, b) -> Efor (i, clean_expr e1, clean_expr e2, clean_bloc b)
  | Ewhile (e, b) -> Ewhile (clean_expr e, clean_bloc b)
  | Eif (e, b, els) -> Eif (clean_expr e, clean_bloc b, clean_else els)
  | _ as a -> a
and clean_else x =
  match x with
  | Iend -> Iend
  | Ielse b -> Ielse (clean_bloc b)
  | Ielseif (e, b, els) -> Ielseif (clean_expr e, clean_bloc b, clean_else els)
and clean_bloc x =
  match x with
  | Bloc l -> Bloc (clean_bloc_bis l)
and clean_bloc_bis x =
  match x with
  | [] -> []
  | Some e :: q -> Some (clean_expr e) :: (clean_bloc_bis q)
  | None :: q -> clean_bloc_bis q
and clean_bloc1 x =
  match x with
  | Bloc1 (e, b) ->
    let b' = match b with
    | None -> None
    | Some b'' -> Some (clean_bloc b'')
    in
    Bloc1 (clean_expr e, b')
;;

*)
