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
