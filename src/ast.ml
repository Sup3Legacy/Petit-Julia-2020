(*
###########################################################
#                                                         #
#                       Ast principal                     #
#                                                         #
###########################################################
*)

open Astype

type position = {ldeb : int; cdeb : int; lfin : int; cfin : int}
[@@deriving show]
;;

(* Exceptions *)
exception Lexing_Error
exception Lexing_Error_Msg of string
exception Lexing_Error_Msg_Pos of string * position
exception Parsing_Error
exception Typing_Error
exception Typing_Error_Msg of string
exception Typing_Error_Msg_Pos of string * position
exception Interp_Error_Msg of string

type ident = string
[@@deriving show]
;;

type fichier =
  | DeclarationList of declaration list
[@@deriving show]
and declaration =
  | Dstruct of bool * position * ident * (param list)
  | Dfonction of position * ident * (param list) * position * pjtype * bloc * string (* docstring *)
  | Dexpr of expression
[@@deriving show]
and param =
  | Param of position * ident * position * pjtype
[@@deriving show]
and expression = position * expr
and expr =
  | Eentier of Int64.t
  | Eflottant of float
  | Echaine of ident
  | Etrue
  | Efalse
  | Earray of expression array
  | EentierIdent of position * Int64.t * ident
  | EentierParG of position * Int64.t * bloc
  | Ebloc1 of bloc
  | EparDIdent of expression * position * ident
  | Eapplication of position * ident * (expression list)
  | Enot of expression
  | Eminus of expression
  | Ebinop of position * Astype.operateur * expression * expression
  | Elvalue of lvalue
  | ElvalueAffect of position * lvalue * expression
  | Ereturn of position * (expression option)
  | Efor of ident * expression * expression * bloc
  | Ewhile of expression * bloc
  | Eif of expression * bloc * else_
[@@deriving show]
and lvalue =
  | Lident of position * ident
  | Lindex of expression * position * ident
  | Larrayindex of expression * position * expression
[@@deriving show]
and else_ =
  | Iend
  | Ielse of bloc
  | Ielseif of expression * bloc * else_
[@@deriving show]
and bloc = position * (expression list)
[@@deriving show]
;;
