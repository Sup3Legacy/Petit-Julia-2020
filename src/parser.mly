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
%nonassoc NOT
%nonassoc UMINUS
%right EXP
%left DOT


%nonassoc ELSE
%nonassoc ELSEIF

%nonassoc WHILE


%start <Ast.fichier> fichier
%%

fichier:
  | declarations = list(decl) EOF {DeclarationList declarations}
;

decl:
  | s = structure SEMICOLON {Dstruct s}
  | f = fonction SEMICOLON {Dfonction f}
  | e = expr SEMICOLON {Dexpr e}
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
    PARD t = typage? b = bloc_END
    {
      Function (ig, parameters, t, b)
    }
;

param:
  | i = IDENT b = typage? {Param (i, b)}
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
  | i = ENTIER_PARG b = bloc1_PARD {EentierParG (i, b)}
  | PARG b = bloc1_PARD {Ebloc1 b}
  | PARG e = expr i = PARD_IDENT {EparDIdent (e, i)}
  | i = IDENT_PARG l = separated_list(COMMA, expr) PARD {Eapplication (i, l)}
  | NOT e = expr {Enot e}
  | MINUS e = expr %prec MINUS {Eminus e}

  | l = lvalue AFFECT e = expr {ElvalueAffect (l, e)}
  | l = lvalue {Elvalue l}
  | RETURN e = expr? {Ereturn e}
  | FOR i = IDENT AFFECT e1 = expr COLON e2 = expr b = bloc_END {
      Efor ((i : ident), e1, e2, b)
    }
  | WHILE e = expr b = bloc_END {
      Ewhile (e, b)
    }
  | IF e = expr b = bloc el = else_exp {
      Eif (e, b, el)
    }
;

lvalue:
  | i = IDENT {Lident (i : ident)}
  | e = expr DOT i = IDENT {Lindex (e, (i : ident))}
;

else_exp:
  | END {Iend}
  | ELSE b = bloc_END {Ielse b}
  | ELSEIF e = expr b = bloc el = else_exp {Ielseif (e, b, el)}
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
  | e = separated_list(SEMICOLON, expr?) {Bloc e}
;

bloc_END:
  |b = bloc_END2 {Bloc b}
;

bloc_END2:
  |e = expr END {[Some e]}
  | END {[]}
  | SEMICOLON bl = bloc_END2 {None::bl}
  | e = expr SEMICOLON bl = bloc_END2 {(Some e)::bl}
;

bloc_PARD:
  |e = expr PARD {[Some e]}
  | PARD {[]}
  | SEMICOLON bl = bloc_PARD {None::bl}
  | e = expr SEMICOLON bl = bloc_PARD {(Some e)::bl}
;

bloc1_PARD:
  | e = expr SEMICOLON b = bloc_PARD {Bloc1 (e, Some (Bloc b))}
  | e = expr PARD {Bloc1 (e, None)}
;

bloc1:
  | e = expr SEMICOLON b = bloc {Bloc1 (e, Some b)}
  | e = expr {Bloc1 (e, None)}
;
