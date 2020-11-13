open Ast

type pjtype =
  | Any
  | Nothing
  | Int64
  | Bool
  | String
  | Struct of string
;;

type function =
