type ident = string
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
  | Function of ident * (param list) * (ident option) * bloc
[@@deriving show]
and param =
  | Param of ident * (ident option)
[@@deriving show]
and expression =
  | Eentier of int
  | Echaine of ident
  | Etrue
  | Efalse
  | EentierIdent of int * ident
  | EentierParG of int * bloc1
  | Ebloc1 of bloc1
  | EparDIdent of expression * ident
  | Eapplication of ident * (expression list)
  | Enot of expression
  | Eminus of expression
  | Ebinop of operateur * expression * expression
  | Elvalue of lvalue
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
