type possibleType = Int | Bool | Void

type singleType = 
  | Type of possibleType
  | ASTVectorType of singleType
  | TypeFun of types * singleType
and types = 
  singleType list   

type singleArg = 
  | ASTSingleArg of string * singleType
and args = 
  singleArg list

type singleArgProc = 
  | ASTSingleArgProc of string * singleType
  | ASTSingleArgProcVar of string * singleType
and argsProc = 
  singleArgProc list


type singleExpr =
  | ASTNum of int
  | ASTId of string
  | ASTIf of singleExpr * singleExpr * singleExpr
  | ASTAnd of singleExpr * singleExpr
  | ASTOr of singleExpr * singleExpr
  | ASTApp of singleExpr * exprsProc
  | ASTLambdaExpression of args * singleExpr
  | ASTAlloc of singleExpr
  | ASTLen of singleExpr
  | ASTNth of singleExpr * singleExpr
  | ASTVset of singleExpr * singleExpr * singleExpr

and exprs =
  singleExpr list

and exprProc = 
  | ASTExpr of singleExpr
  | ASTExprProcAdr of lValue
and exprsProc =
  exprProc list

and stat =
  | ASTEcho of singleExpr
  | ASTSet of lValue * singleExpr
  | ASTIf of singleExpr * block * block
  | ASTWhile of singleExpr * block
  | ASTCall of string * exprsProc
  
and lValue = 
  | ASTLValueIdent of string
  | ASTVectorValue of lValue * singleExpr

and def = 
  | ASTConst of string * singleType * singleExpr
  | ASTFun of string * singleType * args * singleExpr
  | ASTFunRec of string * singleType * args * singleExpr
  | ASTVar of string * singleType
  | ASTProc of string * argsProc * block
  | ASTProcRec of string * argsProc * block
  | ASTFunBlock of string * singleType * argsProc * block
  | ASTFunRecBlock of string * singleType * argsProc * block

and ret = 
  | ASTReturn of singleExpr

and cmds =
  | ASTStat of stat
  | ASTRet of ret
  | ASTDef of def * cmds
  | ASTStatWithCmds of stat * cmds
                
and block = 
  | ASTBlock of cmds