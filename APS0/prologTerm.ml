open Ast

let rec print_singleType stype =
  match stype with
  | Type(Bool)-> Printf.printf "bool"
  | Type(Int)-> Printf.printf "int"
  | TypeFun(types, t) -> (
    Printf.printf "funType";
    Printf.printf "(";
    Printf.printf "[";
    print_types types;
    Printf.printf "]";
    Printf.printf ",";
    print_singleType t;
    Printf.printf ")")
and print_types ts =
  match ts with
  | [] -> ()
  | [a] -> print_singleType a
  | h::tail -> (
    print_singleType h;
    Printf.printf "*";
    print_types tail)

let print_singleArg sarg =
  match sarg with
  | ASTSingleArg (ident, t) -> (
    Printf.printf "singleArg(" ;
    Printf.printf "%s" ident ;
    Printf.printf ":";
    print_singleType t;
    Printf.printf ")")

let rec print_args args =
  match args with
  | [] -> ()
  | [a] -> print_singleArg a
  | h::tail ->
      print_singleArg h;
      Printf.printf ",";
      print_args tail

let rec print_singleExpr e =
  match e with
  | ASTNum n -> Printf.printf "num(%d)" n
  | ASTId x -> Printf.printf "id(%s)" x
  | ASTIf(cond, consequence, alternative) -> (
      Printf.printf "if";
      Printf.printf "(";
      print_singleExpr cond;
      Printf.printf ",";
      print_singleExpr consequence;
      Printf.printf ",";
      print_singleExpr alternative;
      Printf.printf ")";)
  | ASTAnd(op1, op2) -> (
      Printf.printf "and";
      Printf.printf "(";
      print_singleExpr op1;
      Printf.printf ",";
      print_singleExpr op2;
      Printf.printf ")";)
  | ASTOr(op1, op2) -> (
      Printf.printf "or";
      Printf.printf "(";
      print_singleExpr op1;
      Printf.printf ",";
      print_singleExpr op2;
      Printf.printf ")";)
  | ASTApp(e, es) -> (
      Printf.printf "app(";
      print_singleExpr e;
      Printf.printf ",(";
      print_exprs es;
      Printf.printf "))")
  | ASTLambdaExpression(args, singleExpr) -> (
      Printf.printf "lambda";
      Printf.printf "(";
      Printf.printf "[";
      print_args args;
      Printf.printf "]";
      Printf.printf ",";
      print_singleExpr singleExpr;
      Printf.printf ")";)

and print_exprs es =
  match es with
  | [] -> ()
  | [e] ->
      print_singleExpr e
  | e::es -> (
      print_singleExpr e;
      print_char ',';
      print_exprs es)

let print_stat s =
  match s with
  | ASTEcho e -> (
      Printf.printf "echo";
      Printf.printf "(";
      print_singleExpr e;
      Printf.printf ")")

let print_def d =
  match d with
  | ASTConst(id, t, e) -> (
      Printf.printf "const";
      Printf.printf "(";
      Printf.printf "%s," id;
      print_singleType t;
      Printf.printf ",";
      print_singleExpr e;
      Printf.printf ")";)
  | ASTFun(id, t, args, e) -> (
      Printf.printf "fun";
      Printf.printf "(";
      Printf.printf "%s," id;
      Printf.printf ",";
      print_singleType t;
      Printf.printf ",";
      Printf.printf "[";
      print_args args;
      Printf.printf "]";
      Printf.printf ",";
      print_singleExpr e;
      Printf.printf ")")
  | ASTFunRec(id, t, args, e) -> (
      Printf.printf "funRec";
      Printf.printf "(";
      Printf.printf "%s," id;
      Printf.printf ",";
      print_singleType t;
      Printf.printf ",";
      Printf.printf "[";
      print_args args;
      Printf.printf "]";
      Printf.printf ",";
      print_singleExpr e;
      Printf.printf ")")

let rec print_cmds c =
  match c with
  | ASTStat s -> print_stat s
  | ASTDef(def,c) -> (
    Printf.printf "cmds";
    Printf.printf "(";
    print_def def;
    Printf.printf ";";
    print_cmds c;
    Printf.printf ")")

let print_prog p =
  Printf.printf("prog");
  Printf.printf("(");
  Printf.printf("[");
  print_cmds p;
  Printf.printf("]");
  Printf.printf(")");
  Printf.printf(".\n")
;;

let fname = Sys.argv.(1) in (*debugger fait avec l'aide du pro, merci a lui*)
let ic = open_in fname in
try
  let lexbuf = Lexing.from_channel ic in
  let p = 
    try
      Parser.prog Lexer.token lexbuf
    with _ ->
      let open Lexing in 
      let curr = lexbuf.lex_curr_p in 
      let line = curr.pos_lnum in
      let cnum = curr.pos_cnum - curr.pos_bol in
      Printf.printf "Erreur de syntaxe à la ligne %d, colonne %d\n" line cnum;
      exit 1
  in

  print_prog p;
  print_string ".\n"
with Lexer.Eof ->
  exit 0