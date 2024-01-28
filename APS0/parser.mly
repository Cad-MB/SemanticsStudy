%{
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

%}
  
%token <int> NUM
%token <string> IDENT
%token LBRA RBRA
%token LPAR RPAR 
%token CONST FUN REC IF ECHO
%token SEMCOL COL COMA ARROW
%token STAR
%token AND OR
%token BOOL INT

%type <Ast.cmds> prog
%type <Ast.cmds> cmds
%type <Ast.def> def
%type <Ast.stat> stat
%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.singleType> singleType
%type <Ast.types> types
%type <Ast.arg> arg
%type <Ast.args> args

%start prog

%%
prog: LBRA cmds RBRA    { $2 }
;

cmds:
  stat                  { ASTStat $1 }
  | def SEMCOL cmds     { ASTDef($1, $3) }
;

def:
  CONST IDENT singleType expr {ASTConst($2, $3, $4)}
  | FUN IDENT singleType LBRA args RBRA expr  {ASTFunct($2, $3, $5, $7)}
  | FUN REC IDENT singleType LBRA args RBRA expr {ASTRecFunct($3, $4, $6, $8)} 
;


stat:
  ECHO expr             { ASTEcho($2) }
;

expr:
  NUM                   { ASTNum($1) }
| IDENT                 { ASTId($1) }
| LPAR IF expr expr expr RPAR { ASTIf($3, $4, $5)}
| LPAR AND expr expr RPAR {ASTAnd($3, $4)}
| LPAR OR expr expr RPAR  {ASTOr($3, $4)}
| LPAR expr exprs RPAR  { ASTApp($2, $3) }
| LBRA args RBRA expr {ASTLambdaExpression($2, $4)}
;

exprs :
  expr       { [$1] }
| expr exprs { $1::$2 }
;

singleType:
  INT       { Type(Int) }
  | BOOL    { Type(Bool) }  
  | LPAR types ARROW singleType RPAR {TypeFunc($2, $4)}
;

types:
  singleType { ASTType($1)}
| singleType STAR types { ASTTypes($1, $3)}
;

arg: IDENT COL singleType {ASTArg($1, $3)}
;

args:
  arg {ASTOneArg($1)}
| arg COMA args {ASTArgs($1, $3)}
;

