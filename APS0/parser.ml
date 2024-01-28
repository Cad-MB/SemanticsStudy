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
\001\000\002\000\002\000\003\000\003\000\003\000\004\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\006\000\006\000\
\007\000\007\000\007\000\008\000\008\000\009\000\010\000\010\000\
\000\000"

let yylen = "\002\000\
\003\000\001\000\003\000\004\000\007\000\008\000\002\000\001\000\
\001\000\006\000\005\000\005\000\004\000\004\000\001\000\002\000\
\001\000\001\000\005\000\001\000\003\000\003\000\001\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\025\000\000\000\000\000\000\000\000\000\
\000\000\002\000\000\000\000\000\000\000\008\000\009\000\000\000\
\000\000\007\000\001\000\000\000\000\000\018\000\017\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\000\000\000\000\004\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\022\000\024\000\014\000\000\000\000\000\
\000\000\016\000\013\000\021\000\000\000\000\000\000\000\000\000\
\011\000\012\000\019\000\005\000\000\000\010\000\006\000"

let yydgoto = "\002\000\
\004\000\008\000\009\000\010\000\046\000\047\000\035\000\036\000\
\028\000\029\000"

let yysindex = "\005\000\
\006\255\000\000\038\255\000\000\012\255\013\255\039\255\014\255\
\017\255\000\000\008\255\008\255\024\255\000\000\000\000\029\255\
\002\255\000\000\000\000\038\255\008\255\000\000\000\000\039\255\
\030\255\008\255\019\255\020\255\043\255\039\255\039\255\039\255\
\039\255\000\000\027\255\035\255\000\000\029\255\048\255\008\255\
\029\255\039\255\039\255\039\255\039\255\039\255\046\255\008\255\
\008\255\049\255\029\255\000\000\000\000\000\000\039\255\050\255\
\051\255\000\000\000\000\000\000\052\255\039\255\055\255\054\255\
\000\000\000\000\000\000\000\000\039\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\057\255\000\000\000\000\000\000\000\000\
\000\000\000\000\053\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\058\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\034\000\000\000\000\000\249\255\017\000\246\255\018\000\
\000\000\226\255"

let yytablesize = 68
let yytable = "\018\000\
\024\000\025\000\014\000\015\000\016\000\001\000\017\000\050\000\
\003\000\033\000\053\000\030\000\021\000\011\000\012\000\039\000\
\037\000\019\000\031\000\032\000\063\000\013\000\043\000\044\000\
\045\000\026\000\022\000\023\000\020\000\052\000\027\000\040\000\
\038\000\041\000\054\000\055\000\056\000\057\000\061\000\014\000\
\015\000\016\000\048\000\017\000\005\000\006\000\042\000\064\000\
\007\000\049\000\051\000\059\000\062\000\034\000\068\000\065\000\
\066\000\067\000\069\000\070\000\023\000\071\000\058\000\015\000\
\000\000\060\000\000\000\020\000"

let yycheck = "\007\000\
\011\000\012\000\001\001\002\001\003\001\001\000\005\001\038\000\
\003\001\017\000\041\000\010\001\005\001\002\001\002\001\026\000\
\024\000\004\001\017\001\018\001\051\000\009\001\030\000\031\000\
\032\000\002\001\019\001\020\001\012\001\040\000\002\001\013\001\
\003\001\014\001\042\000\043\000\044\000\045\000\049\000\001\001\
\002\001\003\001\016\001\005\001\007\001\008\001\004\001\055\000\
\011\001\015\001\003\001\006\001\004\001\020\000\062\000\006\001\
\006\001\006\001\004\001\006\001\004\001\069\000\046\000\006\001\
\255\255\048\000\255\255\015\001"

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
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 40 "parser.mly"
                        ( _2 )
# 175 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 44 "parser.mly"
                        ( ASTStat _1 )
# 182 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 45 "parser.mly"
                        ( ASTDef(_1, _3) )
# 190 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleType) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 49 "parser.mly"
                              (ASTConst(_2, _3, _4))
# 199 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.singleType) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 50 "parser.mly"
                                              (ASTFunct(_2, _3, _5, _7))
# 209 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.singleType) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 51 "parser.mly"
                                                 (ASTRecFunct(_3, _4, _6, _8))
# 219 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 56 "parser.mly"
                        ( ASTEcho(_2) )
# 226 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 60 "parser.mly"
                        ( ASTNum(_1) )
# 233 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "parser.mly"
                        ( ASTId(_1) )
# 240 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 62 "parser.mly"
                              ( ASTIf(_3, _4, _5))
# 249 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 63 "parser.mly"
                          (ASTAnd(_3, _4))
# 257 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 64 "parser.mly"
                          (ASTOr(_3, _4))
# 265 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 65 "parser.mly"
                        ( ASTApp(_2, _3) )
# 273 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 66 "parser.mly"
                      (ASTLambdaExpression(_2, _4))
# 281 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 70 "parser.mly"
             ( [_1] )
# 288 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 71 "parser.mly"
             ( _1::_2 )
# 296 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
            ( Type(Int) )
# 302 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
            ( Type(Bool) )
# 308 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.singleType) in
    Obj.repr(
# 77 "parser.mly"
                                     (TypeFunc(_2, _4))
# 316 "parser.ml"
               : Ast.singleType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 81 "parser.mly"
             ( ASTType(_1))
# 323 "parser.ml"
               : Ast.types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.singleType) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.types) in
    Obj.repr(
# 82 "parser.mly"
                        ( ASTTypes(_1, _3))
# 331 "parser.ml"
               : Ast.types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.singleType) in
    Obj.repr(
# 85 "parser.mly"
                          (ASTArg(_1, _3))
# 339 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 89 "parser.mly"
      (ASTOneArg(_1))
# 346 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.args) in
    Obj.repr(
# 90 "parser.mly"
                (ASTArgs(_1, _3))
# 354 "parser.ml"
               : Ast.args))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.cmds)
