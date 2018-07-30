
{
open Parser        (* The type token is defined in parser.mli *)
exception EOF
}



let integer =('+'|'-') ['1'-'9'] ['0'-'9']* | ['1'-'9'] ['0'-'9']*
let chars = ['a'-'z' 'A'-'Z' '0'-'9' '_']
rule token = parse
 	integer as int { INT (int_of_string int) }
  | [' ' '\t' '\n']	{ token lexbuf }
  | '.'         				      { EOL 				}
  | ','         					  { COMMA 				}
  | '+'								  { PLUS				}	
  | '-'								  { MINUS				}
  | '*'								  { MULTIPLY			}
  | '/'								  { DIVIDE				}
  | '^'								  { POWER				}
  | '!'          					  { CUT   				} 
  | '='								  { EQUAL				}
  | '>' 							  { GRT					}
  | '<' 							  { LES					}
  | ">="							  { GRTEQ				}
  | "<="							  { LESEQ				}
  | '('                               { LP                  }
  | ')'                               { RP                  }
  | ":-"                              { IFF                 }
  | "fail"							  { FAIL   				}
  | "true"                            { BOOL (true)         }
  | "false"                           { BOOL (false)        }
  | ['A'-'Z']chars*    as id          { VAR (id)            }
  | ['a'-'z']chars*    as id          { ID (id) 			}
  | eof 							  { EOF 				}