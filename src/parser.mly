%{
  open Ast
%}

%token <int> INT
%token <string> CHAINE
%token <string> IDENT
%token EOF PARG PARD
%token AFFECT
%token OR AND
%token EQ NEQ L G LEQ GEQ
%token PLUS MINUS TIMES MODULO EXP
%token NOT
%token DOT

%token ELSE ELSEIF END FALSE FOR FUNCTION IF MUTABLE RETURN STRUCT TRUE WHILE

%token <int * string> ENTIER_IDENT
%token <string> IDENT_PARG
%token <int> ENTIER_PARG
%token <string> PARD_IDENT

%token TYPE

%token COLON SEMICOLON COMMA

%nonassoc RETURN
%right AFFECT
%left OR
%left AND
%left EQ NEQ L G LEQ GEQ
%left PLUS MINUS
%left TIMES MODULO
%nonassoc NOT, unary_minus
%right EXP
%left DOT



%start <Ast.fichier> fichier
%%

fichier:
  | declarations = declarations_list {DeclarationList declarations}
;

declarations_list:
  | EOF {[]}
  | s = structure SEMICOLON d = declarations_list {(Dstruct s)::d}
  | f = fonction SEMICOLON d = declarations_list {(Dfonction f)::d}
  | e = expr SEMICOLON d = declarations_list {(Dexpr e)::d}
  | s = structure EOF {[Dstruct s]}
  | f = fonction EOF {[Dfonction f]}
  | e = expr EOF {[Dexpr e]}
  | SEMICOLON {print_string "semicolon\n";exit 1}
;


structure:
  | b = MUTABLE? STRUCT i = IDENT parameters = param_list
    {
      let res = match b with
        | Some () -> true
        | None -> false
      in
      Struct (res, i, parameters)
    }
;

param_list :
  | END {[]}
  | SEMICOLON pl = param_list {None::pl}
  | p = param SEMICOLON pl = param_list {(Some p)::pl}
  | p = param END {[Some p]}
;

typage:
  | TYPE i = IDENT {i}
;

fonction:
  | FUNCTION ig = IDENT_PARG parameters = separated_list(COMMA, param)
    PARD t = typage? e = expr? b = bloc_END
    {
      Function (ig, parameters, t, Bloc  (e::b))
    }
;

param:
  | i = IDENT b = typage? {Param (i, b)}
;


expr_wMin_:
  | e1 = expr_wMin_ o = operateur e2 = expr {Ebinop (o, e1, e2)}
  | i = INT {Eentier i}
  | s = CHAINE {Echaine s}
  | TRUE {Etrue}
  | FALSE {Efalse}
  | b = ENTIER_IDENT {
      match b with
      | i, s -> EentierIdent (i, s)
    }
  | i = ENTIER_PARG b = bloc1 PARD {EentierParG (i, b)}
  | PARG b = bloc1 PARD {Ebloc1 b}
  | PARG e = expr i = PARD_IDENT {EparDIdent (e, i)}
  | i = IDENT_PARG l = separated_list(COMMA, expr) PARD {Eapplication (i, l)}
  | NOT e = expr {Enot e}
  | l = lvalue_wMin_ AFFECT e = expr {ElvalueAffect (l, e)}
  | l = lvalue_wMin_ {Elvalue l}
  | RETURN e = expr {Ereturn (Some e)}
  | RETURN {Ereturn None}
  | FOR i = IDENT AFFECT e1 = expr COLON e2b = expr_bloc END {
  		let (e2, b) = e2b in
      Efor ((i : ident), e1, e2,Bloc b)
    }
  | w = whileExp {
      let (e, b) = w in
      Ewhile (e, b)
    }
  | IF eb = expr_bloc el = else_exp {
  		let (e,b) = eb in 
	    Eif (e,Bloc b, el)
    }
;

expr_w_Ret:
  | e1 = expr o = operateur e2 = expr_w_Ret {Ebinop (o, e1, e2)}
  | i = INT {Eentier i}
  | s = CHAINE {Echaine s}
  | TRUE {Etrue}
  | FALSE {Efalse}
  | b = ENTIER_IDENT {
      match b with
      | i, s -> EentierIdent (i, s)
    }
  | i = ENTIER_PARG b = bloc1 PARD {EentierParG (i, b)}
  | PARG b = bloc1 PARD {Ebloc1 b}
  | PARG e = expr i = PARD_IDENT {EparDIdent (e, i)}
  | i = IDENT_PARG l = separated_list(COMMA, expr) PARD {Eapplication (i, l)}
  | NOT e = expr_w_Ret {Enot e}
  | l = lvalue AFFECT e = expr_w_Ret {ElvalueAffect (l, e)}
  | l = lvalue {Elvalue l}
  | MINUS e = expr_w_Ret %prec unary_minus{Eminus e}
  | RETURN e = expr_w_Ret {Ereturn (Some e)}
  | FOR i = IDENT AFFECT e1 = expr COLON e2b = expr_bloc END {
  		let (e2, b) = e2b in 
      	Efor ((i : ident), e1, e2,Bloc b)
    }
  | w = whileExp {
      let (e, b) = w in
      Ewhile (e, b)
    }
  | IF eb = expr_bloc el = else_exp {
  		let (e,b) = eb in
      Eif (e,Bloc b, el)
    }

;

expr:
  | e1 = expr o = operateur e2 = expr {Ebinop (o, e1, e2)}
  | i = INT {Eentier i}
  | s = CHAINE {Echaine s}
  | TRUE {Etrue}
  | FALSE {Efalse}
  | b = ENTIER_IDENT {
      match b with
      | i, s -> EentierIdent (i, s)
    }
  | i = ENTIER_PARG b = bloc1 PARD {EentierParG (i, b)}
  | PARG b = bloc1 PARD {Ebloc1 b}
  | PARG e = expr i = PARD_IDENT {EparDIdent (e, i)}
  | i = IDENT_PARG l = separated_list(COMMA, expr) PARD {Eapplication (i, l)}
  | NOT e = expr {Enot e}
  | l = lvalue AFFECT e = expr {ElvalueAffect (l, e)}
  | l = lvalue {Elvalue l}
  | MINUS e = expr %prec unary_minus{Eminus e}
  | RETURN e = expr {Ereturn (Some e)}
  | RETURN {Ereturn None}
  | FOR i = IDENT AFFECT e1 = expr COLON e2b = expr_bloc END {
  		let (e2, b) = e2b in 
      	Efor ((i : ident), e1, e2,Bloc b)
    }
  | w = whileExp {
      let (e, b) = w in
      Ewhile (e, b)
    }
  | IF eb = expr_bloc el = else_exp {
  		let (e,b) = eb in
      Eif (e,Bloc b, el)
    }

;

whileExp:
  | WHILE e = expr b = bloc_END {(e,Bloc b)}
  | WHILE e1 = expr_w_Ret e2 = expr_wMin_ b = bloc_END {(e1,Bloc ((Some e2)::b))}
;

lvalue:
  | i = IDENT {Lident (i : ident)}
  | e = expr DOT i = IDENT {Lindex (e, (i : ident))}
;

lvalue_wMin_:
  | i = IDENT {Lident (i : ident)}
  | e = expr_wMin_ DOT i = IDENT {Lindex (e, (i : ident))}
;

else_exp:
  | END {Iend}
  | ELSE b = bloc_END {Ielse (Bloc (None::b))}
  | ELSE e = expr b = bloc_END {Ielse (Bloc ((Some e)::b))}
  | ELSEIF eb = expr_bloc el = else_exp {
  	let (e,b) = eb in 
  	Ielseif (e,Bloc b, el)
  	}
;

%inline operateur:
  | EQ {Eq}
  | NEQ {Neq}
  | L {Lo}
  | G {Gr}
  | LEQ {Leq}
  | GEQ {Geq}
  | PLUS {Plus}
  | MINUS {Minus}
  | TIMES {Times}
  | MODULO {Modulo}
  | EXP {Exp}
  | AND {And}
  | OR {Or}
;

bloc:
  | e = separated_nonempty_list(SEMICOLON, expr?) {Bloc e}
;


expr_bloc:
	| e = expr {(e, [])}
	| e = expr b = expr_bloc2 {(e, b)}
	| e1 = expr_w_Ret e2 = expr_wMin_ b = expr_bloc2 {(e1, (Some e2)::b)}
	| e1 = expr_w_Ret e2 = expr_wMin_ {(e1, [Some e2])}
;

expr_bloc2:
	| SEMICOLON {[]}
	| SEMICOLON e = expr b = expr_bloc2 {(Some e)::b}
	| SEMICOLON b = expr_bloc2 {None::b}
	| SEMICOLON e = expr {[Some e]}
;

bloc_END:
	| END {[None]}
	| SEMICOLON e = expr b = bloc_END {(Some e)::b}
	| SEMICOLON b = bloc_END {None::b}
;

bloc1:
  | e = expr SEMICOLON b = bloc {Bloc1 (e,Some b)}
  | e = expr {Bloc1 (e,None)}
;
