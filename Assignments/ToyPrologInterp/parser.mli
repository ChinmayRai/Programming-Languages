type token =
  | LP
  | RP
  | EOL
  | IFF
  | COMMA
  | CUT
  | EOF
  | PLUS
  | MINUS
  | MULTIPLY
  | DIVIDE
  | POWER
  | EQUAL
  | GRT
  | LES
  | GRTEQ
  | LESEQ
  | FAIL
  | VAR of (string)
  | ID of (string)
  | INT of (int)
  | BOOL of (bool)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.clause
