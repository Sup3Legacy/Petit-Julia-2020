(*

  AST propre au typage

*)



type pjtype =
  | Any
  | Nothing
  | Int64
  | Bool
  | String
  | S of string
[@@deriving show]
;;

type funct = (pjtype list) * pjtype
[@@deriving show]
;;


type value = (* Type des valeurs dans l'interp *)
  | Vint of int
  | Vstring of string
  | Vbool of bool
  | Vnone
[@@deriving show]
;;
