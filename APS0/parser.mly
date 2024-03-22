%{
open Ast
%}


%token LBRA RBRA
%token LPAR RPAR
%token SEMCOL COL COMA ARROW STAR
%token CONST FUN REC ECHO IF
%token AND OR
%token BOOL INT
%token <int> NUM
%token <string> IDENT

%type <Ast.cmds> prog
%type <Ast.cmds> cmds
%type <Ast.def> def
%type <Ast.singleType> singleType
%type <Ast.singleType list> types
%type <Ast.singleArg> singleArg
%type <Ast.singleArg list> args
%type <Ast.stat> stat
%type <Ast.singleExpr> singleExpr
%type <Ast.singleExpr list> exprs

%start prog

%%
prog: 
  | LBRA cmds RBRA { $2 }
;

cmds:
  | stat { ASTStat($1) }
  | def SEMCOL cmds { ASTDef($1, $3) }
;

def:
  | CONST IDENT singleType singleExpr { ASTConst($2, $3, $4) }
  | FUN IDENT singleType LBRA args RBRA singleExpr  { ASTFun($2, $3, $5, $7) }
  | FUN REC IDENT singleType LBRA args RBRA singleExpr { ASTFunRec($3, $4, $6, $8) } 
;

singleType:
  | INT { Type(Int) }
  | BOOL { Type(Bool) }  
  | LPAR types ARROW singleType RPAR { TypeFun($2, $4) }
;
types:
  | singleType { [$1] }
  | singleType STAR types { $1::$3 }
;

singleArg: 
  | IDENT COL singleType { ASTSingleArg($1, $3) }
;
args:
  | singleArg { [$1] }
  | singleArg COMA args { $1::$3 }
;

stat:
  | ECHO singleExpr { ASTEcho($2) }
;

singleExpr:
  | NUM { ASTNum($1) }
  | IDENT { ASTId($1) }
  | LPAR IF singleExpr singleExpr singleExpr RPAR { ASTIf($3, $4, $5) }
  | LPAR AND singleExpr singleExpr RPAR { ASTAnd($3, $4) }
  | LPAR OR singleExpr singleExpr RPAR { ASTOr($3, $4) }
  | LPAR singleExpr exprs RPAR { ASTApp($2, $3) }
  | LBRA args RBRA singleExpr { ASTLambdaExpression($2, $4) }
;

exprs :
  | singleExpr       { [$1] }
  | singleExpr exprs { $1::$2 }
;