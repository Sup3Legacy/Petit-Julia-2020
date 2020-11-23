(*

  AST principal correspondant aux types de l'arbre syntaxique

*)
open Astype

exception Parsing_Error
exception Typing_Error
exception Typing_Error_Msg of string

type ident = string
[@@deriving show]
;;

type position = {ldeb : int; cdeb : int; lfin : int; cfin : int}
[@@deriving show]
;;

type fichier =
  | DeclarationList of declaration list
[@@deriving show]
and declaration =
  | Dstruct of bool * position * ident * (param list)
  | Dfonction of position * ident * (param list) * position * pjtype * bloc
  | Dexpr of position * expression
[@@deriving show]
and param =
  | Param of position * ident * position * pjtype
[@@deriving show]
and expression = position * expr
and expr =
  | Eentier of  int
  | Echaine of ident
  | Etrue
  | Efalse
  | EentierIdent of  int * ident
  | EentierParG of position * int * bloc
  | Ebloc1 of bloc
  | EparDIdent of  expression * position * ident
  | Eapplication of position * ident * (expression list)
  | Enot of expression
  | Eminus of expression
  | Ebinop of position * operateur * expression * expression
  | Elvalue of lvalue
  | ElvalueAffect of position * lvalue * expression
  | Ereturn of (expression option)
  | Efor of ident * expression * expression * bloc
  | Ewhile of expression * bloc
  | Eif of expression * bloc * else_
[@@deriving show]
and lvalue =
  | Lident of ident
  | Lindex of expression * position * ident
[@@deriving show]
and else_ =
  | Iend
  | Ielse of bloc
  | Ielseif of expression * bloc * else_
[@@deriving show]
and bloc = position * (expression list)
[@@deriving show]
and operateur =
  | Eq
  | Neq
  | Lo
  | Gr
  | Leq
  | Geq
  | Plus
  | Minus
  | Times
  | Modulo
  | Exp
  | And
  | Or
[@@deriving show]
;;
