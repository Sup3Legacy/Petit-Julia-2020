(*
###########################################################
#                                                         #
#                  Ast propre au typage                   #
#                                                         #
###########################################################
*)

type ident = string
[@@deriving show]

type operateur =
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
	| Concat
[@@deriving show]
;;

type pjtype = (* types de petitjulia *)
 	| Any
 	| Nothing
 	| Int64
 	| Float64
 	| Bool
	| String
	| Array
	| Char64
 	| S of string
[@@deriving show]
;;


module Tmap = Map.Make(String)
module Tset = Set.Make(String)
module TypeSet = Set.Make(struct type t = pjtype let compare = compare end)
module TypeMap = Map.Make(struct type t = pjtype let compare = compare end)

type varEnv = (bool * pjtype) Tmap.t
type funcEnv = (int * (pjtype list) * pjtype) list Tmap.t
type structEnv = ((int * pjtype) Tmap.t * int) Tmap.t
type argsEnv = (bool * pjtype * string) Tmap.t

module ISet = Set.Make(Int)


type funcSet = ident * ISet.t

type expressionTyper = pjtype * exprTyper
and exprTyper =
	| EntierE of Int64.t
	| FlottantE of float
	| ChaineE of string
	| TrueE
	| FalseE
	| AssertE of (int * string * expressionTyper)
	| CharE of int
(*	| ArrayE of expressionTyper list
	| EntierIdentE of Int64.t * pjtype * ident * bool
	| EntierParGE of Int64.t * blocTyper*)
	| BlocE of blocTyper
	(*| ParDIdentE of expressionTyper * pjtype * ident * bool*)
	| CallE of funcSet * (expressionTyper list)
	| NotE of expressionTyper
	| MinusE of expressionTyper
	| BinopE of operateur * expressionTyper * expressionTyper
	| LvalueE of lvalueTyper
	| LvalueAffectE of lvalueTyper * expressionTyper
	| ReturnE of pjtype * (expressionTyper option) (* expected type of the return *)
	| ForE of ident * pjtype Tmap.t * expressionTyper * expressionTyper * blocTyper
	| WhileE of expressionTyper * pjtype Tmap.t * blocTyper
	| DoWhileE of  pjtype Tmap.t * blocTyper
	| IfE of expressionTyper * blocTyper * elseTyper
and lvalueTyper =
	| IdentL of pjtype * ident * bool
	| IndexL of expressionTyper * ident * ident (* expression, name of struct, name of value *)
	| ArrayL of expressionTyper * expressionTyper
and elseTyper =
	| EndI
	| ElseI of blocTyper
	| ElseifI of expressionTyper * blocTyper * elseTyper
and blocTyper = pjtype * (expressionTyper list)
;;

module Imap = Map.Make(Int)

type funct =
	|Funct of ((ident * pjtype) list * pjtype Tmap.t * blocTyper * pjtype)
	|StructBuilder of ((ident * pjtype) list)
and funcMap = funct Imap.t Tmap.t

type fichierTyper = exprTyper list * pjtype Tmap.t * structEnv * funcMap * funcEnv
;;
