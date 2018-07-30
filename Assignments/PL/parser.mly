%{
  open PL_6
%}

%token LP RP EOL IFF COMMA CUT EOF PLUS MINUS MULTIPLY DIVIDE POWER CUT EQUAL GRT LES FAIL
%token <string> VAR
%token <string> ID
%token <int>	INT
%token <bool>   BOOL


%left EQUAL GRT LES 
%left PLUS MINUS        /* lowest precedence */
%left MULTIPLY 
%left DIVIDE         /* medium precedence */
%left POWER 


%start main             /* the entry point */ 
%type <PL_6.clause> main


%%

main:
	clause EOL 				{$1							}
	| EOF      				{Fact(N(("File_end",0),[]))	}
;

clause:
	term       				{		Fact($1	)			}	
	| term IFF 	termlist	{		Rule($1,$3)			}
	| exp					{ 		Arith($1)			}
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

exp:
	exp EQUAL exp			{ Eq($1,$3)					}
	| exp GRT exp			{ Gt($1,$3)					}
	| exp LES exp			{ Lt($1,$3)					}
	| exp PLUS exp			{ Plus($1,$3)				}
	| exp MINUS exp			{ Minus($1,$3)				}
	| exp MULTIPLY exp		{ Multiply($1,$3)			}
	| exp DIVIDE exp		{ Div($1,$3)				}
	| exp POWER exp			{ Power($1,$3)				}
	| INT  					{ Nat($1)					}
;	
