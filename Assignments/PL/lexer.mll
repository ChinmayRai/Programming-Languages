
{
open Parser        (* The type token is defined in parser.mli       ('+'|'-') ['1'-'9'] ['0'-'9']* |    *)
exception EOF
}



let integer = ['1'-'9'] ['0'-'9']*
let chars = ['a'-'z' 'A'-'Z' '0'-'9' '_']
rule token = parse
 	integer as int        { INT (int_of_string int) }
  | [' ' '\t']+	        { token lexbuf }
  | '\n'                { token lexbuf }
  | '.'         				{ EOL          }
  | ','         				{ COMMA 			 }
  | '+'								  { PLUS				 }	
  | '-'								  { MINUS				 }
  | '*'								  { MULTIPLY		 }
  | '/'								  { DIVIDE			 }
  | '^'								  { POWER				 }
  | '!'          				{ CUT   			 } 
  | '='								  { EQUAL				 }
  | '>' 							  { GRT					 }
  | '<' 							  { LES					 }
  | '('                 { LP           }
  | ')'                 { RP           }
  | ":-"                { IFF          }
  | "fail"							{ FAIL   			 }
  | "true"              { BOOL (true)  }
  | "false"             {BOOL (false)  }
  | eof                 { EOF          }
  | ['A'-'Z']chars*    as ip          { VAR (ip)      }
  | ['a'-'z']chars*    as id          {  ID(id)       }

  (*

  | '%'                 { line lexbuf  }
  

  and line = parse
  | eof  { token lexbuf }
  | '\n' { token lexbuf }
  | _    { line lexbuf  }

  *)