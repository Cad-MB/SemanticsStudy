(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)



type possibleType = Int | Bool
type singleType = 
      Type of (possibleType) 
    | TypeFunc of types * singleType (*ARROW*) 
  and types = 
      ASTType of singleType 
    | ASTTypes of singleType * types (* multiple types a * b  *)

type arg = 
    ASTArg of string * singleType

type args = 
    ASTOneArg of arg
  | ASTArgs of arg * args


type expr =
    ASTNum of int
  | ASTId of string
  | ASTApp of expr * expr list
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTLambdaExpression of args * expr

type stat =
    ASTEcho of expr
      
type def =
  ASTConst of string * singleType * expr
  | ASTFunct of string * singleType * args * expr
  | ASTRecFunct of string * singleType * args * expr

type cmds =
    ASTStat of stat
  | ASTDef of def * cmds

	
