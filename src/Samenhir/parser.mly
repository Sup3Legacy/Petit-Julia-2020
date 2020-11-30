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
	|head = HEADER? tL = list(tokenDecl) aS = list(assocDecl) s = startDecl SPLIT rL = list(rulesDecl) EOF {
		let h = match head with 
			|None -> ""
			|Some s -> s
		in let rList = List.concat rL in 
		let gramR = {startR = s; raw_rules = rList}
		in {header = h; g = gramR; prio = aS; tokenList = List.concat tL} 
	}
;

tokenDecl:
	|TOKEN t = TYPE term = TERM {[term,Some t]}
	|TOKEN tL = list(TERM) {List.map (fun t -> (t,None)) tL}
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

rulesDecl:
	|nt = N_TERM_IDENT t = TYPE POINTS rl = list(rule) SEMICOLON {
		List.map (fun (tL,c,op) -> (nt,t,tL,op,c)) rl
	}
;

rule:
	| VERT tL = list(ruleElement) c = CODE {(tL,c,None)}
	| VERT tL = list(ruleElement) PREC nt = N_TERM_IDENT c = CODE {(tL,c,Some nt)}
;

ruleElement:
	|t1 = N_TERM_IDENT ASSOC t2 = N_TERM_IDENT { AssocNonTerminal (t1, t2) }
	|t1 = N_TERM_IDENT ASSOC t2 = TERM { AssocTerminal (t1, t2) }
	|t = TERM { TerminalR t }
	|t = N_TERM_IDENT {NonTerminalR t}
;

