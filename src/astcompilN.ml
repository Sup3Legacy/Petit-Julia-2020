open Astype

module TypeMap = Map.Make(struct type t = pjtype let compare = compare end)

type functArbr =
	| Failure
	| Appels of functArbr TypeMap.t
	| Feuille of string * int

type label =
	| Dec of int
	| Tag of ident;;

type expression =
	| Entier of Int64.t
	| Flottant of float
	| Chaine of string
	| True
	| False
	| Nothing
	| Bloc of bloc
(*	| EntierIdent of Int64.t * label
	| EntierParG of Int64.t * bloc
	| ParDIdent of expression * label*)
	| Call of ident * functArbr * (expression list)
	| Not of expression
	| Minus of expression
	| Binop of operateur * expression * expression
	| Ident of label
	| Index of expression * ident * int
	| Array of expression * expression
	| LvalueAffectV of label * expression
	| LvalueAffectI of expression * ident * int * expression
	| LvalueAffectA of expression * expression * expression
	| Ret of pjtype * expression (* expected type of the return *)
	| For of int * int * expression * expression * bloc
	| While of expression * int * int * bloc
	| If of expression * bloc * else_
and else_ =
	| End
	| Else of bloc
	| Elseif of expression * bloc * else_
and bloc = (expression list)
;;

module Imap = Map.Make(Int)

(*type functBuilded =
	|Funct of (int * bloc)
	|StructBuilder of int
and funcBMap = functBuilded Imap.t Tmap.t*)

type fichier = expression list * int * pjtype Tmap.t * funcMap
;;
