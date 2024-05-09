%{
open Ast
%}


%token LBRA RBRA
%token LPAR RPAR
%token SEMCOL COL COMA ARROW STAR
%token CONST FUN REC VAR PROC
%token ECHO SET IF WHILE CALL VOID
%token AND OR
%token BOOL INT
%token VAR
%token ADR VARADR
%token VEC ALLOC NTH LEN VSET // APS2
%token RETURN
%token <int> NUM
%token <string> IDENT

%type <Ast.block> prog
%type <Ast.block> block
%type <Ast.cmds> cmds
%type <Ast.def> def
%type <Ast.singleType> singleType
%type <Ast.singleType list> types
%type <Ast.singleArg> singleArg
%type <Ast.singleArg list> args
%type <Ast.stat> stat
%type <Ast.singleExpr> singleExpr
%type <Ast.singleExpr list> exprs
%type <Ast.exprProc> exprProc
%type <Ast.exprProc list> exprsProc
%type <Ast.ret> ret

%start prog

%%
prog: 
  | block { $1 }
;

block: 
  | LBRA cmds RBRA { ASTBlock($2) }
;

cmds:
  | stat { ASTStat($1) }
  | ret { ASTRet($1)}
  | def SEMCOL cmds { ASTDef($1, $3) }
  | stat SEMCOL cmds { ASTStatWithCmds($1, $3) }
;

ret:
  | RETURN singleExpr { ASTReturn($2) }

def:
  | CONST IDENT singleType singleExpr { ASTConst($2, $3, $4) }
  | FUN IDENT singleType LBRA args RBRA singleExpr  { ASTFun($2, $3, $5, $7) }
  | FUN REC IDENT singleType LBRA args RBRA singleExpr { ASTFunRec($3, $4, $6, $8) } 
  | VAR IDENT singleType { ASTVar($2, $3) }
  | PROC IDENT LBRA argsProc RBRA block  { ASTProc($2, $4, $6) }
  | PROC REC IDENT LBRA argsProc RBRA block { ASTProcRec($3, $5, $7) }
  | FUN IDENT singleType LBRA argsProc RBRA block { ASTFunBlock($2, $3, $5, $7) }
  | FUN REC IDENT singleType LBRA argsProc RBRA block { ASTFunRecBlock($3, $4, $6, $8) } 
;

singleType:
  | INT {Type(Int)}
  | BOOL {Type(Bool)}
  | VOID { Type(Void) }  
  | LPAR VEC singleType RPAR {ASTVectorType($3)}
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

singleArgProc:
  | IDENT COL singleType { ASTSingleArgProc($1, $3) }
  | VARADR IDENT COL singleType { ASTSingleArgProcVar($2, $4) }

argsProc:
  | singleArgProc {[$1]}
  | singleArgProc COMA argsProc {$1::$3}

stat:
  | ECHO singleExpr { ASTEcho($2) }
  | SET lValue singleExpr { ASTSet($2, $3) }
  | IF singleExpr block block { ASTIf($2, $3, $4) }
  | WHILE singleExpr block { ASTWhile($2, $3) }
  | CALL IDENT exprsProc { ASTCall($2, $3) }
;

lValue:
  | IDENT {ASTLValueIdent($1)}
  | LPAR NTH lValue singleExpr RPAR {ASTVectorValue($3, $4)}

exprProc:
  | singleExpr            {ASTExpr($1)}
  | LPAR ADR lValue RPAR   {ASTExprProcAdr($3)}

exprsProc:
  | exprProc          {[$1]}
  | exprProc exprsProc { $1::$2 }

singleExpr:
  | NUM { ASTNum($1) }
  | IDENT { ASTId($1) }
  | LPAR IF singleExpr singleExpr singleExpr RPAR { ASTIf($3, $4, $5) }
  | LPAR AND singleExpr singleExpr RPAR { ASTAnd($3, $4) }
  | LPAR OR singleExpr singleExpr RPAR { ASTOr($3, $4) }
  | LPAR singleExpr exprsProc RPAR { ASTApp($2, $3) }
  | LBRA args RBRA singleExpr { ASTLambdaExpression($2, $4) }
  | LPAR ALLOC singleExpr RPAR {ASTAlloc($3)}
  | LPAR LEN singleExpr RPAR {ASTLen($3)}
  | LPAR NTH singleExpr singleExpr RPAR {ASTNth($3, $4)}
  | LPAR VSET singleExpr singleExpr singleExpr RPAR {ASTVset($3, $4, $5)}
;

exprs :
  | singleExpr       { [$1] }
  | singleExpr exprs { $1::$2 }
;