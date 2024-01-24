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

%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.cmd list> cmds
%type <Ast.cmd list> prog

%start prog

%%
prog: LBRA cmds RBRA    { $2 }
;

cmds:
  stat                  { [ASTStat $1] }
  // | def SEMCOL cmds     { $3 }
;

// def:
//   CONST IDENT type expr {}
//   | FUN IDENT type LBRA args RBRA expr  {}
//   | FUN REC IDENT type LBRA args RBRA expr {} 
// ;

// type:
//   BOOL    {ASTBool()}
//   | INT   {}
//   | LPAR types ARROW type   {}
// ;

// types:
//   type  {}
//   | type * types   {}
// ;

// args:
//   arg   {}
//   | arg COMA args  {}
// ;

// arg: 
//   ident COL type
// ;

stat:
  ECHO expr             { ASTEcho($2) }
;

type:
BOOL OR int {}
| LPAR types ARROW type RPAR

types:
type {}
| type STAR types {}

args:
arg {}
| arge COMA args {}

arg: IDENT COL type {}

expr:
  NUM                   { ASTNum($1) }
| IDENT                 { ASTId($1) }
| LPAR IF expr expr expr RPAR { ASTIf($3, $4, $5)}
| LPAR AND expr expr RPAR {ASTAnd($3, $4)}
| LPAR OR expr expr RPAR  {ASTOr($3, $4)}
| LPAR expr exprs RPAR  { ASTApp($2, $3) }
// | RBRA args LBRA expr {}
;

exprs :
  expr       { [$1] }
| expr exprs { $1::$2 }
;

