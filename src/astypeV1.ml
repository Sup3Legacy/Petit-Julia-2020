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
  | Float64
  | Bool
  | String
  | S of string
  | Array of pjtype
[@@deriving show]
;;

type funct = (pjtype list) * pjtype
[@@deriving show]
;;

module Tmap = Map.Make(String)
module Tset = Set.Make(String)
module TypeSet = Set.Make(struct type t = pjtype let compare = compare end)
module FuncSet = Set.Make(struct type t = funct let compare = compare end)

type varEnv = (bool * pjtype) Tmap.t
type funcEnv = funct list Tmap.t
type structEnv = pjtype Tmap.t Tmap.t
type argsEnv = (bool * pjtype * string) Tmap.t
