(*
###########################################################
#                                                         #
#                   Ast de l'interpréteur                 #
#                                                         #
###########################################################
*)

open Format

(* Types des valeurs *)
type structure =
  string * bool * (string list) * ((string, value) Hashtbl.t) (* Le premier string est le nom de la structure *)
and value =
  | Vint of Int64.t
  | Vbool of bool
  | Vstring of string
  | Vfloat of float (* Futur *)
  | Vstruct of structure
  | Vnothing
;;

module Imap = Map.Make(String);; (* Map contenant les environnements typage *)
type varEnv = value Imap.t
type funcEnv = ((Ast.ident * (Ast.param list) * Astype.pjtype * Ast.bloc) list) Imap.t
type structEnv = (bool * Ast.ident * (Ast.param list)) Imap.t
