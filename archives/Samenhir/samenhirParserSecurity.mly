%{
	open SamenhirAst
%}

%token <string> CODE
%token <string> HEADER

%token VERT
%token <string> TERM
%token <string> N_TERM_IDENT
%token <string> TYPE
%token ASSOC START SPLIT TOKEN POINTS SEMICOLON
%token RIGHT LEFT NONASSOC PREC
%token EOF

%start <SamenhirAst.parser	> program


%%

program:
	|h = HEADER tL = list_tokenDecl aS = list_assocDecl s = startDecl SPLIT rL = list_rulesDecl EOF {
		let rList = List.concat rL in 
		let gramR = {startR = s; raw_rules = rList}
		in {header = h; g = gramR; prio = aS; tokenList = List.concat tL} 
	}
	|tL = list_tokenDecl aS = list_assocDecl s = startDecl SPLIT rL = list_rulesDecl EOF {
		let rList = List.concat rL in 
		let gramR = {startR = s; raw_rules = rList}
		in {header = ""; g = gramR; prio = aS; tokenList = List.concat tL} 
	}
;

list_tokenDecl:
	| {[]}
	| t = tokenDecl tL = list_tokenDecl {t::tL}

tokenDecl:
	|TOKEN t = TYPE term = TERM {[term,Some t]}
	|TOKEN tL = list_TERM {List.map (fun t -> (t,None)) tL}
;

list_TERM:
	| t = TERM {[t]}
	| t = TERM tL = list_TERM {t::tL}
;

list_assocDecl:
	| {[]}
	| a= assocDecl aL = list_assocDecl {a::aL}
;

assocDecl:
	|a = associativity t = TERM {(a,t)}
	|a = associativity t = N_TERM_IDENT {(a,t)}
;

associativity:
	|LEFT { Left }
	|RIGHT { Right }
	|NONASSOC { NonAssoc }
;

startDecl:
	|START nt = N_TERM_IDENT {nt}
;
list_rulesDecl:
	| {[]}
	| r = rulesDecl rL = list_rulesDecl {r::rL}
;

rulesDecl:
	|nt = N_TERM_IDENT t = TYPE POINTS rl = list_rule_SEMICOLON {
		List.map (fun (tL,c,op) -> (nt,t,tL,op,c)) rl
	}
;

list_rule_SEMICOLON:
	| r = rule SEMICOLON {[r]}
	| r = rule rL = list_rule_SEMICOLON {r::rL}
;

rule:
	| VERT tL = list_ruleElement c = CODE {(tL,c,None)}
	| VERT tL = list_ruleElement PREC nt = N_TERM_IDENT c = CODE {(tL,c,Some nt)}
;


list_ruleElement:
	| {[]}
	| r = ruleElement rL = list_ruleElement {r::rL}
;

ruleElement:
	|t1 = N_TERM_IDENT ASSOC t2 = N_TERM_IDENT { AssocNonTerminal (t1, t2) }
	|t1 = N_TERM_IDENT ASSOC t2 = TERM { AssocTerminal (t1, t2) }
	|t = TERM { TerminalR t }
	|t = N_TERM_IDENT {NonTerminalR t}
;

