%{
  open Ast
  open Astype

%}

%token <Ast.position*int> INT
%token <Ast.position*string> CHAINE
%token <Ast.position*string> IDENT
%token EOF
%token <Ast.position> PARG
%token <Ast.position> PARD
%token AFFECT
%token OR AND
%token EQ NEQ L G LEQ GEQ
%token PLUS MINUS TIMES MODULO EXP
%token <Ast.position> NOT
%token DOT

%token <Ast.position> ELSE
%token <Ast.position> ELSEIF
%token <Ast.position> END
%token <Ast.position> FALSE
%token <Ast.position> FOR
%token <Ast.position> FUNCTION
%token <Ast.position> IF
%token MUTABLE
%token <Ast.position> RETURN
%token <Ast.position> STRUCT
%token <Ast.position> TRUE
%token <Ast.position> WHILE

%token <Ast.position*int * string> ENTIER_IDENT
%token <Ast.position*string> IDENT_PARG
%token <Ast.position*int> ENTIER_PARG
%token <Ast.position*string> PARD_IDENT

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
;


structure:
  | b = MUTABLE? STRUCT pi = IDENT parameters = param_list
    {
      let (p,i) = pi in
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
  | TYPE pi = IDENT {snd pi}
;

fonction:
  | FUNCTION pig = IDENT_PARG parameters = separated_list(COMMA, param)
    PARD t = typage? e = expr? b = bloc_END
    {
    	let (p,ig) = pig in
      let typ =
      match t with
      | Some "Int64" -> Int64
      | Some "Bool" -> Bool
      | Some "String" -> String
      | Some s -> S s
      | None -> Any
      in
      Function (ig, parameters, typ, Bloc  (e::b))
    }
;

param:
  | pi = IDENT b = typage?
    {
      let (p,i) = pi in
      let typ =
      match b with
      | Some "Int64" -> Int64
      | Some "Bool" -> Bool
      | Some "String" -> String
      | Some s -> S s
      | None -> Any
      in
      Param (i, typ)
    }
;


expr_wMin_:
  | e1 = expr_wMin_ o = operateur e2 = expr {Ebinop (o, e1, e2)}
  | pi = INT {let (p,i) = pi in Eentier (p,i)}
  | ps = CHAINE {let (p,s) = ps in Echaine (p,s)}
  | p = TRUE {Etrue p}
  | p = FALSE {Efalse p}
  | pb = ENTIER_IDENT {
  	  let (p,i,s) = pb in EentierIdent (p, i, s)
    }
  | pi = ENTIER_PARG b = bloc1 PARD {let (p,i) = pi in EentierParG (p, i, b)}
  | PARG b = bloc1 PARD {Ebloc1 b}
  | PARG e = expr pi = PARD_IDENT {let (p,i) = pi in EparDIdent (p, e, i)}
  | pi = IDENT_PARG l = separated_list(COMMA, expr) PARD {let (p,i) = pi in Eapplication (p, i, l)}
  | NOT e = expr {Enot e}
  | pl = lvalue_wMin_ AFFECT e = expr {let (p,l) = pl in ElvalueAffect (l, e)}
  | pl = lvalue_wMin_ {let (p,l) = pl in Elvalue (p,l)}
  | RETURN e = expr {Ereturn (Some e)}
  | RETURN {Ereturn None}
  | FOR pi = IDENT AFFECT e1 = expr COLON e2b = expr_bloc END {
  		let (p,i) = pi in
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
  | pi = INT {let (p,i) = pi in Eentier (p,i)}
  | ps = CHAINE {let (p,s) = ps in Echaine (p,s)}
  | p = TRUE {Etrue p}
  | p = FALSE {Efalse p}
  | pb = ENTIER_IDENT {
  	  let (p,i,s) = pb in EentierIdent (p, i, s)
    }
  | pi = ENTIER_PARG b = bloc1 PARD {let (p,i) = pi in EentierParG (p, i, b)}
  | PARG b = bloc1 PARD {Ebloc1 b}
  | PARG e = expr pi = PARD_IDENT {let (p,i) = pi in EparDIdent (p, e, i)}
  | pi = IDENT_PARG l = separated_list(COMMA, expr) PARD {let (p,i) = pi in Eapplication (p, i, l)}
  | NOT e = expr_w_Ret {Enot e}
  | pl = lvalue AFFECT e = expr_w_Ret {let (p,l) = pl in ElvalueAffect (l, e)}
  | pl = lvalue {let (p,l) = pl in Elvalue (p,l)}
  | MINUS e = expr_w_Ret %prec unary_minus{Eminus e}
  | RETURN e = expr_w_Ret {Ereturn (Some e)}
  | FOR pi = IDENT AFFECT e1 = expr COLON e2b = expr_bloc END {
  		let (p,i) = pi in
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
  | pi = INT {let (p,i) = pi in Eentier (p,i)}
  | ps = CHAINE {let (p,s) = ps in Echaine (p,s)}
  | p = TRUE {Etrue p}
  | p = FALSE {Efalse p}
  | pb = ENTIER_IDENT {
  	  let (p,i,s) = pb in EentierIdent (p, i, s)
    }
  | pi = ENTIER_PARG b = bloc1 PARD {let (p,i) = pi in EentierParG (p, i, b)}
  | PARG b = bloc1 PARD {Ebloc1 b}
  | PARG e = expr pi = PARD_IDENT {let (p,i) = pi in EparDIdent (p, e, i)}
  | pi = IDENT_PARG l = separated_list(COMMA, expr) PARD {let (p,i) = pi in Eapplication (p, i, l)}
  | NOT e = expr {Enot e}
  | pl = lvalue AFFECT e = expr {let (p,l) = pl in ElvalueAffect (l, e)}
  | pl = lvalue {let (p,l) = pl in Elvalue (p,l)}
  | MINUS e = expr %prec unary_minus{Eminus e}
  | RETURN e = expr {Ereturn (Some e)}
  | RETURN {Ereturn None}
  | FOR pi = IDENT AFFECT e1 = expr COLON e2b = expr_bloc END {
  		let (p,i) = pi in
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
  | pi = IDENT {let (p,i) = pi in p,Lident (i : ident)}
  | e = expr DOT pi = IDENT {let (p,i) = pi in p,Lindex (e, (i : ident))}
;

lvalue_wMin_:
  | pi = IDENT {let (p,i) = pi in p,Lident (i : ident)}
  | e = expr_wMin_ DOT pi = IDENT {let (p,i) = pi in p,Lindex (e, (i : ident))}
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
