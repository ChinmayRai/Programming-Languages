type token =
  | Id of (string)
  | TRUE
  | FALSE
  | OR
  | AND
  | XOR
  | NEG
  | LPAREN
  | RPAREN
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "beParser.mly"
open BeAst
# 18 "beParser.ml"
let yytransl_const = [|
  258 (* TRUE *);
  259 (* FALSE *);
  260 (* OR *);
  261 (* AND *);
  262 (* XOR *);
  263 (* NEG *);
  264 (* LPAREN *);
  265 (* RPAREN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* Id *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\004\000\004\000\
\004\000\004\000\004\000\000\000"

let yylen = "\002\000\
\003\000\001\000\003\000\001\000\003\000\001\000\001\000\001\000\
\001\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\009\000\007\000\008\000\000\000\000\000\000\000\
\000\000\000\000\006\000\011\000\000\000\000\000\000\000\000\000\
\010\000\000\000\000\000\005\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\011\000"

let yysindex = "\006\000\
\010\255\000\000\000\000\000\000\000\000\010\255\010\255\255\254\
\008\255\001\255\000\000\000\000\000\255\010\255\010\255\010\255\
\000\000\008\255\001\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\015\000\
\005\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\008\000\002\000\000\000"

let yygindex = "\000\000\
\009\000\007\000\004\000\250\255"

let yytablesize = 273
let yytable = "\012\000\
\004\000\003\000\014\000\014\000\002\000\016\000\001\000\001\000\
\017\000\020\000\003\000\004\000\005\000\015\000\012\000\013\000\
\006\000\007\000\019\000\000\000\018\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\003\000\004\000\003\000\
\002\000\004\000\003\000\001\000\000\000\002\000\000\000\000\000\
\001\000"

let yycheck = "\006\000\
\000\000\000\000\004\001\004\001\000\000\005\001\001\000\000\000\
\009\001\016\000\001\001\002\001\003\001\006\001\000\000\007\000\
\007\001\008\001\015\000\255\255\014\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\004\001\004\001\006\001\006\001\
\004\001\009\001\009\001\004\001\255\255\009\001\255\255\255\255\
\009\001"

let yynames_const = "\
  TRUE\000\
  FALSE\000\
  OR\000\
  AND\000\
  XOR\000\
  NEG\000\
  LPAREN\000\
  RPAREN\000\
  EOF\000\
  "

let yynames_block = "\
  Id\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : BeAst.bexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'xexpr) in
    Obj.repr(
# 16 "beParser.mly"
                     ( Or(_1,_3) )
# 163 "beParser.ml"
               : BeAst.bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'xexpr) in
    Obj.repr(
# 17 "beParser.mly"
                     ( _1 )
# 170 "beParser.ml"
               : BeAst.bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'xexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'nexpr) in
    Obj.repr(
# 19 "beParser.mly"
                       ( Xor(_1,_3) )
# 178 "beParser.ml"
               : 'xexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'nexpr) in
    Obj.repr(
# 20 "beParser.mly"
                     ( _1 )
# 185 "beParser.ml"
               : 'xexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'nexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 22 "beParser.mly"
                      ( And(_1,_3) )
# 193 "beParser.ml"
               : 'nexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 23 "beParser.mly"
                     ( _1 )
# 200 "beParser.ml"
               : 'nexpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 25 "beParser.mly"
                    ( Const(true) )
# 206 "beParser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    Obj.repr(
# 26 "beParser.mly"
                    ( Const(false) )
# 212 "beParser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 27 "beParser.mly"
                    ( Var(_1) )
# 219 "beParser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : BeAst.bexpr) in
    Obj.repr(
# 28 "beParser.mly"
                         ( _2 )
# 226 "beParser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 29 "beParser.mly"
                    ( Neg(_2) )
# 233 "beParser.ml"
               : 'atom))
(* Entry expr *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let expr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : BeAst.bexpr)
