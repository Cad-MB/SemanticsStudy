type possibleType = Int | Bool | Void (* type void n'existe pas dans le formulaire mais le prof nous a dit qu'il faut l'ajouter *)

type singleType = 
  Type of possibleType 
  | TypeFun of types * singleType
and types = 
  singleType list

type singleArg = 
  | ASTSingleArg of string * singleType
and args = 
  singleArg list

type singleExpr =
  | ASTNum of int
  | ASTId of string
  | ASTIf of singleExpr * singleExpr * singleExpr
  | ASTAnd of singleExpr * singleExpr
  | ASTOr of singleExpr * singleExpr
  | ASTApp of singleExpr * singleExpr list
  | ASTLambdaExpression of args * singleExpr
and exprs =
  singleExpr list
  
and stat =
  | ASTEcho of singleExpr
  | ASTSet of string * singleExpr
  | ASTIf of singleExpr * block * block
  | ASTWhile of singleExpr * block
  | ASTCall of string * exprs
  
and def = 
  | ASTConst of string * singleType * singleExpr
  | ASTFun of string * singleType * args * singleExpr
  | ASTFunRec of string * singleType * args * singleExpr
  | ASTVar of string * singleType
  | ASTProc of string * args * block
  | ASTProcRec of string * args * block
  
and cmds =
  | ASTStat of stat
  | ASTDef of def * cmds
  | ASTStatWithCmds of stat * cmds
                
and block = 
  | ASTBlock of cmds