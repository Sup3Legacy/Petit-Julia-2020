%{
  open Ast
  open Astype
  open Hyper2

%}

%token <Ast.position*int> INT
%token <Ast.position*string> CHAINE
%token <Ast.position*string> IDENT
%token EOF
%token <Ast.position> PARG
%token <Ast.position> PARD
%token <Ast.position> AFFECT
%token <Ast.position> OR
%token <Ast.position> AND
%token <Ast.position> EQ
%token <Ast.position> NEQ
%token <Ast.position> L
%token <Ast.position> G
%token <Ast.position> LEQ
%token <Ast.position> GEQ
%token <Ast.position> PLUS
%token <Ast.position> MINUS
%token <Ast.position> TIMES
%token <Ast.position> MODULO
%token <Ast.position> EXP
%token <Ast.position> NOT
%token <Ast.position> DOT

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

%token <Ast.position> COLON
%token <Ast.position> SEMICOLON
%token <Ast.position> COMMA

%token <string> DOCSTRING

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
  | s = structure SEMICOLON d = declarations_list {s::d}
  | f = fonction SEMICOLON d = declarations_list {f::d}
  | e = expr SEMICOLON d = declarations_list {(Dexpr e)::d}
  | s = structure EOF {[s]}
  | f = fonction EOF {[f]}
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
      Dstruct (res, p, i, parameters)
    }
;

param_list :
  | END {[]}
  | SEMICOLON pl = param_list {pl}
  | p = param SEMICOLON pl = param_list {p::pl}
  | p = param END {[p]}
;

typage:
  | TYPE pi = IDENT {pi}
;

fonction:
  | doc = DOCSTRING? FUNCTION pig = IDENT_PARG parameters = separated_list(COMMA, param)
    PARD pt = typage? e = expr? pb = bloc_END
    {
      let docstring =
        match doc with
        | None -> "No docstring associated to this function"
        | Some s -> s
      in
    	let (p1,ig) = pig in
      let (pEnd, (p,eL)) = pb in
      let b = match e with
        | None -> p, eL
        | Some e -> Hyper2.fusionPos (fst e) p, e::eL
      in let typ,p2 = match pt with
        | Some (p, "Int64") -> Int64, p
        | Some (p, "Bool") -> Bool, p
        | Some (p, "String") -> String, p
        | Some (p, "Any") -> Any, p
        | Some (p, "Nothing") -> Nothing, p
        | Some (p, s) -> S s, p
        | None -> Any, p1
      in
      Dfonction (p1,ig, parameters, p2, typ, b, docstring)
    }
;

param:
  | pi = IDENT pb = typage?
    {
      let (p1,i) = pi in
      let typ,p2 = match pb with
        | Some (p, "Int64") -> Int64, p
        | Some (p, "Bool") -> Bool, p
        | Some (p, "String") -> String, p
        | Some (p, "Any") -> Any, p
        | Some (p, "Nothing") -> Nothing, p
        | Some (p, s) -> S s, p
        | None -> Any, p1
      in
      Param (p1, i, p2, typ)
    }
;


expr_wMin_:
  | pe1 = expr_wMin_ po = operateur pe2 = expr {
      let (p1, e1) = pe1 in
      let (p,o) = po in
      let (p2,e2) = pe2 in
      (Hyper2.fusionPos p1 p2) , Ebinop (p, o, pe1, pe2) }
  | pi = INT {let (p,i) = pi in p,Eentier i}
  | ps = CHAINE {let (p,s) = ps in p, Echaine s}
  | p = TRUE {p, Etrue}
  | p = FALSE {p, Efalse}
  | pb = ENTIER_IDENT {
  	  let (p,i,s) = pb in p, EentierIdent (p, i, s)
    }
  | pi = ENTIER_PARG pb = bloc1 p2 = PARD {
      let (p,i) = pi in
      Hyper2.fusionPos p p2, EentierParG (p, i, pb)}
  | p1 = PARG pb = bloc1 p2 = PARD { Hyper2.fusionPos p1 p2, Ebloc1 pb }
  | p1 = PARG e = expr pi = PARD_IDENT {
    let (p2,i) = pi in
    Hyper2.fusionPos p1 p2, EparDIdent (e, p2, i)}
  | pi = IDENT_PARG l = separated_list(COMMA, expr) p2 = PARD {
    let (p,i) = pi in
    Hyper2.fusionPos p p2, Eapplication (p, i, l)}
  | p = NOT e = expr {Hyper2.fusionPos p (fst e), Enot e}
  | pl = expr_wMin_ AFFECT e = expr {
  		match pl with
  			|p,Elvalue l -> Hyper2.fusionPos p (fst e), ElvalueAffect (p, l, e)
  			|_ -> raise Ast.Parsing_Error
  		}
  | pl = lvalue_wMin_ {let (p,l) = pl in p, Elvalue l}
  | p = RETURN e = expr {Hyper2.fusionPos p (fst e), Ereturn (p,Some e)}
  | p = RETURN {p, Ereturn (p, None)}
  | p1 = FOR pi = IDENT AFFECT e1 = expr COLON e2b = expr_bloc p2 = END {
  		let (p,i) = pi in
  		let (e2, b) = e2b in
      Hyper2.fusionPos p1 p2, Efor ((i : ident), e1, e2, b)
    }
  | w = whileExp { w }
  | p1 = IF eb = expr_bloc pel = else_exp {
  		let (e, b) = eb in
      let (p2, el) = pel in
	    Hyper2.fusionPos p1 p2, Eif (e, b, el)
    }
;

expr_w_Ret:
  | e1 = expr po = operateur e2 = expr_w_Ret {
    let (p, o) = po in
    (Hyper2.fusionPos (fst e1) (fst e2), Ebinop (p, o, e1, e2))}
  | pi = INT {let (p,i) = pi in p, Eentier i}
  | ps = CHAINE {let (p,s) = ps in p, Echaine s}
  | p = TRUE {p, Etrue}
  | p = FALSE {p, Efalse}
  | pb = ENTIER_IDENT {
  	  let (p,i,s) = pb in p, EentierIdent (p, i, s)
    }
  | pi = ENTIER_PARG pb = bloc1 p2 = PARD {
    let (p1,i) = pi in
    (Hyper2.fusionPos p1 p2, EentierParG (p1, i, pb))}
  | p1 = PARG b = bloc1 p2 = PARD {Hyper2.fusionPos p1 p2, Ebloc1 b}
  | p1 = PARG e = expr pi = PARD_IDENT {
    let (p2,i) = pi in
    Hyper2.fusionPos p1 p2, EparDIdent (e, p2, i)}
  | pi = IDENT_PARG l = separated_list(COMMA, expr) p2 = PARD {
    let (p1,i) = pi in
    Hyper2.fusionPos p1 p2, Eapplication (p1, i, l)}
  | p = NOT e = expr_w_Ret {Hyper2.fusionPos p (fst e), Enot e}
  | pl = expr AFFECT e = expr_w_Ret {
  		match pl with
  			|p, Elvalue l -> Hyper2.fusionPos p (fst e), ElvalueAffect (p, l, e)
  			| _ -> raise Ast.Parsing_Error
  		}
  | pl = lvalue {let (p,l) = pl in p, Elvalue l}
  | p1 = MINUS e = expr_w_Ret %prec unary_minus{Hyper2.fusionPos p1 (fst e),Eminus e}
  | p1 = RETURN e = expr_w_Ret {Hyper2.fusionPos p1 (fst e), Ereturn (p1, Some e)}
  | p1 = FOR pi = IDENT AFFECT e1 = expr COLON e2b = expr_bloc p2 = END {
  		let (p,i) = pi in
  		let (e2, b) = e2b in
      Hyper2.fusionPos p1 p2,	Efor ((i : ident), e1, e2, b)
    }
  | w = whileExp { w }
  | p1 = IF eb = expr_bloc pel = else_exp {
  		let (e,b) = eb in
      let (p2, el) = pel in
      Hyper2.fusionPos p1 p2, Eif (e, b, el)
    }

;

expr:
  | e1 = expr po = operateur e2 = expr {
    let (p, o) = po in Hyper2.fusionPos (fst e1) (fst e2), Ebinop (p, o, e1, e2)}
  | pi = INT {let (p,i) = pi in p, Eentier i}
  | ps = CHAINE {let (p,s) = ps in p, Echaine s}
  | p = TRUE {p, Etrue}
  | p = FALSE {p, Efalse}
  | pb = ENTIER_IDENT {
  	  let (p,i,s) = pb in p, EentierIdent (p, i, s)
    }
  | pi = ENTIER_PARG b = bloc1 p2 = PARD {
    let (p1,i) = pi in
    Hyper2.fusionPos p1 p2, EentierParG (p1, i, b)}
  | p1 = PARG b = bloc1 p2 = PARD {Hyper2.fusionPos p1 p2, Ebloc1 b}
  | p1 = PARG e = expr pi = PARD_IDENT {
    let (p2,i) = pi in
    Hyper2.fusionPos p1 p2, EparDIdent (e, p2, i)}
  | pi = IDENT_PARG l = separated_list(COMMA, expr) p2 = PARD {
    let (p1,i) = pi in
    Hyper2.fusionPos p1 p2, Eapplication (p1, i, l)}
  | p = NOT e = expr {Hyper2.fusionPos p (fst e), Enot e}
  | pl = expr AFFECT e = expr {
  		match pl with
  			|p, Elvalue l -> Hyper2.fusionPos p (fst e), ElvalueAffect (p, l, e)
  			| _ -> raise Ast.Parsing_Error
  		}
  | pl = lvalue {let (p,l) = pl in p, Elvalue l}
  | p1 = MINUS e = expr %prec unary_minus {Hyper2.fusionPos p1 (fst e), Eminus e}
  | p1 = RETURN e = expr {Hyper2.fusionPos p1 (fst e), Ereturn (p1, Some e)}
  | p = RETURN {p, Ereturn (p, None)}
  | p1 = FOR pi = IDENT AFFECT e1 = expr COLON e2b = expr_bloc p2 = END {
  		let (p,i) = pi in
  		let (e2, b) = e2b in
      Hyper2.fusionPos p1 p2,	Efor ((i : ident), e1, e2, b)
    }
  | w = whileExp { w }
  | p1 = IF eb = expr_bloc pel = else_exp {
  		let (e,b) = eb in
      let (p2, el) = pel in
      Hyper2.fusionPos p1 p2, Eif (e, b, el)
    }

;

whileExp:
  | p1 = WHILE e = expr pb = bloc_END {
    let (p2,b) = pb in
    Hyper2.fusionPos p1 p2, Ewhile (e, b)}
  | p1 = WHILE e1 = expr_w_Ret e2 = expr_wMin_ pb = bloc_END {
    let (p2,(p,eL)) = pb in
    Hyper2.fusionPos p1 p2, Ewhile (e1, (Hyper2.fusionPos (fst e2) p, e2::eL))}
;

lvalue:
  | pi = IDENT {let (p,i) = pi in p,Lident (p,i)}
  | e = expr DOT pi = IDENT {
    let (p,i) = pi in
    Hyper2.fusionPos (fst e) p, Lindex (e, p, (i : ident))}
;

lvalue_wMin_:
  | pi = IDENT {let (p,i) = pi in p,Lident (p,i)}
  | e = expr_wMin_ DOT pi = IDENT {
    let (p,i) = pi in
    Hyper2.fusionPos (fst e) p, Lindex (e, p, (i : ident))}
;

else_exp:
  | p = END {p, Iend}
  | ELSE pb = bloc_END {
    let (p2, b) = pb in
    p2, Ielse b}
  | ELSE e = expr pb = bloc_END {
    let (p2, (p,b)) = pb in
    p2, Ielse (Hyper2.fusionPos (fst e) p, e::b)}
  | ELSEIF eb = expr_bloc el = else_exp {
    let (p, el) = el in
  	let (e,b) = eb in
  	p, Ielseif (e, b, el)
  	}
;

%inline operateur:
  | p = EQ {(p,Eq)}
  | p = NEQ {(p,Neq)}
  | p = L {(p,Lo)}
  | p = G {(p,Gr)}
  | p = LEQ {(p,Leq)}
  | p = GEQ {(p,Geq)}
  | p = PLUS {(p,Plus)}
  | p = MINUS {(p,Minus)}
  | p = TIMES {(p,Times)}
  | p = MODULO {(p,Modulo)}
  | p = EXP {(p,Exp)}
  | p = AND {(p,And)}
  | p = OR {(p,Or)}
;

bloc:
  | e = expr {fst e, [e]}
  | e = expr SEMICOLON pb = bloc {
      let (p, eL) = pb in
      Hyper2.fusionPos (fst e) p, e::eL}
  | p = SEMICOLON pb = bloc {
      let (p2, eL) = pb in
      Hyper2.fusionPos p p2, eL
  }
;


expr_bloc:
	| e = expr {(e, (fst e, []))}
	| e = expr b = expr_bloc2 {(e, b)}
	| e1 = expr_w_Ret pe2 = expr_wMin_ pb = expr_bloc2 {
    let (p1, e2) = pe2 in
    let (p2, eL) = pb in
    (e1, (Hyper2.fusionPos p1 p2, pe2::eL))}
	| e1 = expr_w_Ret pe2 = expr_wMin_ {
    let (p, e2) = pe2 in
    (e1, (p, [pe2]))}
;

expr_bloc2:
	| p = SEMICOLON {p, []}
	| p1 = SEMICOLON e = expr pb = expr_bloc2 {
    let (p2, eL) = pb in
    Hyper2.fusionPos p1 p2, (e::eL)}
	| p1 = SEMICOLON pb = expr_bloc2 {
    let p2,eL = pb in
    Hyper2.fusionPos p1 p2, eL}
	| p = SEMICOLON e = expr {
    Hyper2.fusionPos p (fst e), [e]}
;

bloc_END:
	| p = END {p,(p,[])}
	| p1 = SEMICOLON e = expr pb = bloc_END {
    let pEnd, (p2, eL) = pb in
    pEnd, (Hyper2.fusionPos p1 p2, (e::eL))
  }
	| p1 = SEMICOLON pb = bloc_END {
    let pEnd, (p2, eL) = pb in
    pEnd, (Hyper2.fusionPos p1 p2, eL)}
;

bloc1:
  | e = expr pb = bloc1bis {
    let (p, eL) = pb in
    Hyper2.fusionPos (fst e) p, (e::eL)}
  | e = expr {(fst e), [e]}
;

bloc1bis:
  | p = SEMICOLON {
    (p, [])
  }
  | p = SEMICOLON e = expr {
    (Hyper2.fusionPos p (fst e), [e])
  }
  | p = SEMICOLON pb = bloc1bis {
    let (p2,eL) = pb in (Hyper2.fusionPos p p2, eL)
  }
  | p = SEMICOLON e = expr pb = bloc1bis {
    let (p2,eL) = pb in (Hyper2.fusionPos p p2, e::eL)
  }
;
