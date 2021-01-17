
val typerCompilateur : Ast.fichier -> Astype.varEnv ref -> Astype.funcEnv ref -> Astype.structEnv ref -> Astype.argsEnv ref -> Astype.fichierTyper

val typerRepl : Ast.fichier -> Astype.varEnv ref -> Astype.funcEnv ref -> Astype.structEnv ref -> Astype.argsEnv ref -> Astype.fichierTyper

val resetVE : Astype.varEnv ref -> unit

val resetFE : Astype.funcEnv ref -> unit

val resetSE : Astype.structEnv ref -> unit

val resetAE : Astype.argsEnv ref -> unit
