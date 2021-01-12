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
  | Dstruct of structure
  | Dfonction of fonction
  | Dexpr of expression
[@@deriving show]
and structure =
  | Struct of bool * ident * (param option list)
[@@deriving show]
and fonction =
  | Function of ident * (param list) * pjtype * bloc
[@@deriving show]
and param =
  | Param of ident * pjtype
[@@deriving show]
and expression =
  | Eentier of position * int
  | Echaine of position * ident
  | Etrue of position
  | Efalse of position
  | EentierIdent of position * int * ident
  | EentierParG of position * int * bloc1
  | Ebloc1 of bloc1
  | EparDIdent of position * expression * ident
  | Eapplication of position * ident * (expression list)
  | Enot of expression
  | Eminus of expression
  | Ebinop of operateur * expression * expression
  | Elvalue of position * lvalue
  | ElvalueAffect of lvalue * expression
  | Ereturn of (expression option)
  | Efor of ident * expression * expression * bloc
  | Ewhile of expression * bloc
  | Eif of expression * bloc * else_
[@@deriving show]
and lvalue =
  | Lident of ident
  | Lindex of expression * ident
[@@deriving show]
and else_ =
  | Iend
  | Ielse of bloc
  | Ielseif of expression * bloc * else_
[@@deriving show]
and bloc =
  | Bloc of (expression option) list
[@@deriving show]
and bloc1 =
  | Bloc1 of expression * (bloc option)
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
