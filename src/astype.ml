(*
###########################################################
#                                                         #
#                  Ast propre au typage                   #
#                                                         #
###########################################################
*)

type pjtype = (* types de petitjulia *)
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
