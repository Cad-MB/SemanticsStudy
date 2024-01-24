type token =
  | NUM of (int)
  | IDENT of (string)
  | LBRA
  | RBRA
  | LPAR
  | RPAR
  | CONST
  | FUN
  | REC
  | IF
  | ECHO
  | SEMCOL
  | COL
  | COMA
  | ARROW
  | STAR
  | AND
  | OR
  | BOOL
  | INT

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017                          == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: parser.mly                                                  == *)
(* == Analyse syntaxique                                                   == *)
(* ========================================================================== *)

open Ast

# 39 "parser.ml"
let yytransl_const = [|
  259 (* LBRA *);
  260 (* RBRA *);
  261 (* LPAR *);
  262 (* RPAR *);
  263 (* CONST *);
  264 (* FUN *);
  265 (* REC *);
  266 (* IF *);
  267 (* ECHO *);
  268 (* SEMCOL *);
  269 (* COL *);
  270 (* COMA *);
  271 (* ARROW *);
  272 (* STAR *);
  273 (* AND *);
  274 (* OR *);
  275 (* BOOL *);
  276 (* INT *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\004\000\003\000\005\000\001\000\001\000\001\000\001\000\001\000\
\001\000\002\000\002\000\000\000"

let yylen = "\002\000\
\003\000\001\000\002\000\001\000\001\000\006\000\005\000\005\000\
\004\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\012\000\000\000\000\000\002\000\004\000\
\005\000\000\000\003\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\011\000\009\000\000\000\007\000\008\000\006\000"

let yydgoto = "\002\000\
\020\000\021\000\006\000\004\000\007\000"

let yysindex = "\003\000\
\254\254\000\000\252\254\000\000\019\255\011\255\000\000\000\000\
\000\000\001\255\000\000\000\000\019\255\019\255\019\255\019\255\
\019\255\019\255\019\255\019\255\010\255\019\255\016\255\017\255\
\000\000\000\000\020\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\021\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\251\255\005\000\000\000\000\000\000\000"

let yytablesize = 27
let yytable = "\011\000\
\003\000\008\000\009\000\001\000\016\000\010\000\005\000\017\000\
\018\000\019\000\013\000\022\000\023\000\024\000\012\000\026\000\
\027\000\014\000\015\000\008\000\009\000\028\000\029\000\010\000\
\025\000\030\000\010\000"

let yycheck = "\005\000\
\003\001\001\001\002\001\001\000\010\000\005\001\011\001\013\000\
\014\000\015\000\010\001\017\000\018\000\019\000\004\001\006\001\
\022\000\017\001\018\001\001\001\002\001\006\001\006\001\005\001\
\020\000\006\001\006\001"

let yynames_const = "\
  LBRA\000\
  RBRA\000\
  LPAR\000\
  RPAR\000\
  CONST\000\
  FUN\000\
  REC\000\
  IF\000\
  ECHO\000\
  SEMCOL\000\
  COL\000\
  COMA\000\
  ARROW\000\
  STAR\000\
  AND\000\
  OR\000\
  BOOL\000\
  INT\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 34 "parser.mly"
                        ( _2 )
# 144 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 38 "parser.mly"
                        ( [ASTStat _1] )
# 151 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 69 "parser.mly"
                        ( ASTEcho(_2) )
# 158 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 73 "parser.mly"
                        ( ASTNum(_1) )
# 165 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 74 "parser.mly"
                        ( ASTId(_1) )
# 172 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 75 "parser.mly"
                              ( ASTIf(_3, _4, _5))
# 181 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 76 "parser.mly"
                          (ASTAnd(_3, _4))
# 189 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 77 "parser.mly"
                          (ASTOr(_3, _4))
# 197 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 78 "parser.mly"
                        ( ASTApp(_2, _3) )
# 205 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 83 "parser.mly"
             ( [_1] )
# 212 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 84 "parser.mly"
             ( _1::_2 )
# 220 "parser.ml"
               : Ast.expr list))
(* Entry prog *)
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
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.cmd list)
