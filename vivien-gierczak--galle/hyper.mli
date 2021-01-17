
val separate_int_ident : string -> (Int64.t * string)

val enableEnd : unit -> unit

val disableEnd : unit -> unit

val dernierEstElse : bool ref
val canEnd : bool ref

val enterPar : unit -> unit

val leavePar : unit -> bool

val position : Lexing.lexbuf -> Ast.position

val rajoutePosition : Parser.token -> Lexing.lexbuf -> Parser.token

val file : string ref

val treat_ident : string -> string

val push_to_string_stack : char -> unit 

val empty_stack : unit -> string

val keywords : (string, Parser.token) Hashtbl.t
