
type expr =
	| Mul of expr * expr
	| Int of int
	| Add of expr * expr
