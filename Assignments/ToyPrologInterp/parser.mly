%{
  open Ast
%}

%token LP RP EOL IFF COMMA CUT EOF PLUS MINUS MULTIPLY DIVIDE POWER CUT EQUAL GRT LES GRTEQ LESEQ FAIL
%token <string> VAR
%token <string> ID
%token <int>	INT
%token <bool>   BOOL


%start main             /* the entry point */
%type <Ast.clause> main

%%

main:
	clause main				{$1							}
	| EOF      				{Fact(N(("File_end",0),[]))	}
;


clause:
	term EOL  				{		Fact($1	)			}	
	| term IFF termlist EOL {		Rule($1,$3)			}
;

term:
	ID 						{ N(($1,0),[]) 				} 
	| LP term RP			{ $2						}
	| VAR 					{ Var($1) 					}
	| CUT					{ CUT						}
	| FAIL					{ FAIL						}
	| ID LP termlist RP 	{ N(($1,List.length ($3)),$3)}
;

termlist:
	term 					{ [$1]						}
	| term COMMA termlist	{ $1::$3					}
;


