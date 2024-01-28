(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: prologTerm.ml                                               == *)
(* ==  Génération de termes Prolog                                         == *)
(* ========================================================================== *)
open Ast
  
let stringify t =
  match t with 
    | Int -> "int"
    | Bool -> "bool"


let rec print_singleType stype =
  match stype with 
    | Type(t)-> Printf.printf " %s " (stringify t)
    | TypeFunc(types, t) -> (
      Printf.printf "FuncType(";
      print_types types;
      Printf.printf " -> ";
      print_singleType t;
      Printf.printf ")"
    )

  and print_types ts = 
    match ts with 
        ASTType(t) -> print_singleType t
      | ASTTypes(t, types) -> (
          print_singleType t;
          Printf.printf " * "; 
          print_types types;
        )
      
let print_arg arg =
  match arg with
    ASTArg (ident, t) -> (
      Printf.printf "Arg(" ;
      Printf.printf "%s" ident ;
      Printf.printf ":";
      print_singleType t;
      Printf.printf ")"
      )

let rec print_args args =
  match args with
      ASTOneArg a -> print_arg a
    | ASTArgs(a, argss) -> (
      print_arg a;
      Printf.printf "," ;
      print_args argss
    )


let rec print_expr e =
  match e with
      ASTNum n -> Printf.printf"num(%d)" n
    | ASTId x -> Printf.printf"id(%s)" x
    | ASTApp(e, es) -> (
        Printf.printf"app(";
        print_expr e;
        Printf.printf",[";
        print_exprs es;
        Printf.printf"])"
      )
    | ASTIf(cond, consequence, alternative) -> (
        Printf.printf"if ";
        print_expr cond;
        Printf.printf", consequence ";
        print_expr consequence;
        Printf.printf", alternative ";
        print_expr alternative;
      )
    | ASTAnd(op1, op2) -> (
        Printf.printf"and ";
        Printf.printf"operand 1 ";
        print_expr op1;
        Printf.printf"operand 2 ";
        print_expr op2;
      )
    | ASTOr(op1, op2) -> (
        Printf.printf"or ";
        Printf.printf"operand 1 ";
        print_expr op1;
        Printf.printf"operand 2 ";
        print_expr op2;
      )
    | ASTLambdaExpression(args, expr) -> (
        Printf.printf "Lambda ([";
        print_args args;
        Printf.printf "],";
        Printf.printf " init(";
        print_expr expr;
        Printf.printf ")";
        Printf.printf ")"
    )

and print_exprs es =
  match es with
      [] -> ()
    | [e] -> print_expr e
    | e::es -> (
	print_expr e;
	print_char ',';
	print_exprs es
      )

let print_stat s =
  match s with
      ASTEcho e -> (
	Printf.printf("echo(");
	print_expr(e);
	Printf.printf(")")
      )

let print_def d =
  match d with
      ASTConst(id, t, e) -> (
        Printf.printf "CONST( ";
        Printf.printf "%s," id;
        print_singleType t; 
        Printf.printf ",";
        print_expr e;
        Printf.printf ")";
      )
    | ASTFunct(id, t, args, e) -> (
        Printf.printf "Funtion(";
        Printf.printf "%s," id;
        print_singleType t;
        Printf.printf "[";
        print_args args;
        Printf.printf "]";
        print_expr e;
        Printf.printf ")"
    )
    | ASTRecFunct(id, t, args, e) -> (
        Printf.printf "RecFuntion(";
        Printf.printf "rec ";
        Printf.printf "%s," id;
        print_singleType t;
        Printf.printf "[";
        print_args args;
        Printf.printf "]";
        print_expr e;
        Printf.printf ")"
    )

let rec print_cmds c =
  match c with
      ASTStat s -> print_stat s
    | ASTDef(def,c) -> (
      print_def def; 
      Printf.printf " : ";
      print_cmds c
    )
	
let print_prog p =
  Printf.printf("prog([");
  print_cmds p;
  Printf.printf("])")
;;
	
let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      print_prog p;
      print_string ".\n"
  with Lexer.Eof ->
    exit 0
      
