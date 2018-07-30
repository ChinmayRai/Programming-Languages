
{
open Parser        (* The type token is defined in parser.mli *)
exception EOF
}



let integer =('+'|'-') ['1'-'9'] ['0'-'9']* | ['1'-'9'] ['0'-'9']*
let chars = ['a'-'z' 'A'-'Z' '0'-'9' '_']
rule token = parse
 	integer as int        { INT (int_of_string int) }
  | [' ' '\t']+	        { token lexbuf }
  | '\n'                { token lexbuf }
  | '.'         				{ let () = Printf.printf "EOL\n" in  EOL}
  | ','         				{ let () = Printf.printf "COMMA\n" in COMMA 			 }
  | '+'								  { PLUS				 }	
  | '-'								  { MINUS				 }
  | '*'								  { MULTIPLY		 }
  | '/'								  { DIVIDE			 }
  | '^'								  { POWER				 }
  | '!'          				{ CUT   			 } 
  | '='								  { EQUAL				 }
  | '>' 							  { GRT					 }
  | '<' 							  { LES					 }
  | ">="							  { GRTEQ				 }
  | "<="							  { LESEQ				 }
  | '('                 { let () = Printf.printf "LP\n" in LP }
  | ')'                 { let () = Printf.printf "RP\n" in RP }
  | ":-"                { IFF          }
  | "fail"							{ FAIL   			 }
  | "true"              { BOOL (true)  }
  | "false"             {BOOL (false)  }
  | eof                 { EOF          }
  | ['A'-'Z']chars*    as ip          { VAR (ip)      }
  | ['a'-'z']chars*    as id          { let () = Printf.printf "ID(id)\n" in ID(id)}











  (*

  | '%'                 { line lexbuf  }
  

  and line = parse
  | eof  { token lexbuf }
  | '\n' { token lexbuf }
  | _    { line lexbuf  }

  *)