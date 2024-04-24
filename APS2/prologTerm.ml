open Ast

let rec print_singleType stype =
  match stype with
  | Type(Bool)-> Printf.printf "bool"
  | Type(Int)-> Printf.printf "int"
  | Type(Void)-> Printf.printf "void" (* type void n'existe pas dans le formulaire mais le prof nous a dit qu'il faut l'ajouter *)
  | ASTVectorType(t) -> (
    Printf.printf "vectorType";
    Printf.printf "(";
    print_singleType t;
    Printf.printf ")")
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
    Printf.printf ",";
    print_types tail)

let print_singleArg sarg =
  match sarg with
  | ASTSingleArg (ident, t) -> (
    Printf.printf "(";
    Printf.printf "%s" ident;
    Printf.printf ",";
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

let print_singleArg_proc sarg =
  match sarg with
  | ASTSingleArgProc (ident, t) -> (
    Printf.printf "(";
    Printf.printf "arg";
    Printf.printf "(";
    Printf.printf "%s" ident;
    Printf.printf ")";
    Printf.printf ",";
    print_singleType t;
    Printf.printf ")")
  | ASTSingleArgProcVar (ident, t) -> ( 
    Printf.printf "(";
    Printf.printf "argVar";
    Printf.printf "(";
    Printf.printf "%s" ident;
    Printf.printf ")";
    Printf.printf ",";
    print_singleType t;
    Printf.printf ")"
  )

let rec print_args_proc argsp =
  match argsp with
  | [] -> ()
  | [a] -> print_singleArg_proc a
  | h::tail ->
      print_singleArg_proc h;
      Printf.printf ",";
      print_args_proc tail

let rec print_singleExpr e =
  match e with
  | ASTNum n -> Printf.printf "num(%d)" n
  | ASTId id -> Printf.printf "id(%s)" id
  | ASTIf(cond, consequence, alternative) -> (
      Printf.printf "if";
      Printf.printf "(";
      print_singleExpr cond;
      Printf.printf ",";
      print_singleExpr consequence;
      Printf.printf ",";
      print_singleExpr alternative;
      Printf.printf ")")
  | ASTAnd(op1, op2) -> (
      Printf.printf "and";
      Printf.printf "(";
      print_singleExpr op1;
      Printf.printf ",";
      print_singleExpr op2;
      Printf.printf ")")
  | ASTOr(op1, op2) -> (
      Printf.printf "or";
      Printf.printf "(";
      print_singleExpr op1;
      Printf.printf ",";
      print_singleExpr op2;
      Printf.printf ")")
  | ASTApp(e, es) -> (
      Printf.printf "app(";
      print_singleExpr e;
      Printf.printf ",";
      Printf.printf "[";
      print_exprs es;
      Printf.printf "]";
      Printf.printf ")")
  | ASTLambdaExpression(args, singleExpr) -> (
      Printf.printf "lambda";
      Printf.printf "(";
      Printf.printf "[";
      print_args args;
      Printf.printf "]";
      Printf.printf ",";
      print_singleExpr singleExpr;
      Printf.printf ")")
  | ASTAlloc e -> (
      Printf.printf "alloc";
      Printf.printf "(";
      print_singleExpr e;
      Printf.printf ")")
  | ASTLen e -> (
      Printf.printf "len";
      Printf.printf "(";
      print_singleExpr e;
      Printf.printf ")")
  | ASTNth (e1, e2) -> (
      Printf.printf "nth";
      Printf.printf "(";
      print_singleExpr e1;
      Printf.printf ",";
      print_singleExpr e2;
      Printf.printf ")")
  | ASTVset (e1, e2, e3) -> (
      Printf.printf "vset";
      Printf.printf "(";
      print_singleExpr e1;
      Printf.printf ",";
      print_singleExpr e2;
      Printf.printf ",";
      print_singleExpr e3;
      Printf.printf ")")


and print_exprs es =
  match es with
  | [] -> ()
  | [e] -> print_singleExpr e
  | e::es -> (
      print_singleExpr e;
      print_char ',';
      print_exprs es)

and print_expr_proc ep =
  match ep with
  | ASTExprProcAdr(ident) ->
    Printf.printf "exprProcAdr";
    Printf.printf "(";
    Printf.printf "%s" ident;
    Printf.printf ")"
  | ASTExpr(e) -> print_singleExpr e

and print_exprs_proc eps =
  match eps with
  | [] -> ()
  | [ep] -> print_expr_proc ep
  | ep::eps -> (
    print_expr_proc ep;
    Printf.printf ",";
    print_exprs_proc eps
  )

and print_lValue lvalue =
  match lvalue with
  | ASTLValueIdent ident -> Printf.printf"id(%s)" ident
  | ASTVectorValue (lv, e) -> 
    Printf.printf "vectorValue";
    Printf.printf "(";
    print_lValue lv;
    Printf.printf ",";
    print_singleExpr e;
    Printf.printf ")"

and print_stat s =
  match s with
  | ASTEcho e -> (
      Printf.printf "echo";
      Printf.printf "(";
      print_singleExpr(e);
      Printf.printf ")")
  | ASTSet (lv, expr) -> 
      Printf.printf "set";
      Printf.printf "(";
      print_lValue lv;
      Printf.printf ",";
      print_singleExpr expr;
      Printf.printf ")"
  | ASTIf (cond, consequence, alternative) -> 
      Printf.printf "if";
      Printf.printf "(";
      print_singleExpr cond;
      Printf.printf ",";
      print_block consequence;
      Printf.printf ",";
      print_block alternative;
      Printf.printf ")"
  | ASTWhile (cond, corps) -> 
      Printf.printf "while";
      Printf.printf "(";
      print_singleExpr cond;
      Printf.printf ",";
      print_block corps;
      Printf.printf ")"
  | ASTCall (ident, expressions) -> 
      Printf.printf "call";
      Printf.printf "(";
      Printf.printf "id(%s)" ident;
      Printf.printf ",";
      Printf.printf "[";
      print_exprs_proc expressions;
      Printf.printf "]";
      Printf.printf ")"
      

and print_def d =
  match d with
  | ASTConst(ident, t, e) -> (
      Printf.printf "const";
      Printf.printf "(";
      Printf.printf "%s" ident;
      Printf.printf ",";
      print_singleType t;
      Printf.printf ",";
      print_singleExpr e;
      Printf.printf ")")
  | ASTFun(ident, t, args, e) -> (
      Printf.printf "fun";
      Printf.printf "(";
      Printf.printf "%s" ident;
      Printf.printf ",";
      print_singleType t;
      Printf.printf ",";
      Printf.printf "[";
      print_args args;
      Printf.printf "]";
      Printf.printf ",";
      print_singleExpr e;
      Printf.printf ")")
  | ASTFunRec(ident, t, args, e) -> (
      Printf.printf "funRec";
      Printf.printf "(";
      Printf.printf "%s" ident;
      Printf.printf ",";
      print_singleType t;
      Printf.printf ",";
      Printf.printf "[";
      print_args args;
      Printf.printf "]";
      Printf.printf ",";
      print_singleExpr e;
      Printf.printf ")")
  | ASTVar(ident, t) -> 
      Printf.printf "var";
      Printf.printf "(";
      Printf.printf "%s" ident;
      Printf.printf ",";
      print_singleType t;
      Printf.printf ")"
  | ASTProc(ident, args, corps) -> 
      Printf.printf "proc";
      Printf.printf "(";
      Printf.printf "%s" ident;
      Printf.printf ",";
      Printf.printf "[";
      print_args_proc args;
      Printf.printf "]";
      Printf.printf ",";
      print_block corps;
      Printf.printf ")"
  | ASTProcRec(ident, args, corps) ->
      Printf.printf "procRec";
      Printf.printf "(";
      Printf.printf "%s" ident;
      Printf.printf ",";
      Printf.printf "[";
      print_args_proc args;
      Printf.printf "]";
      Printf.printf ",";
      print_block corps;
      Printf.printf ")"

and print_cmds c =
  match c with
  ASTStat s -> print_stat s
  | ASTDef(def,c) -> (
    Printf.printf "cmds";
    Printf.printf "(";
    print_def def;
    Printf.printf ")";
    Printf.printf ",";
    print_cmds c)
  | ASTStatWithCmds(stat,cmds) -> (
    print_stat stat;
    Printf.printf ",";
    print_cmds cmds)

and print_block b =
  match b with 
  ASTBlock block -> (
    Printf.printf("block");
    Printf.printf("(");
    Printf.printf("[");
    print_cmds block;
    Printf.printf("]");
    Printf.printf(")")
  )

  let print_prog p =
  Printf.printf("prog");
  Printf.printf("(");
  print_block p;
  Printf.printf(")")
;;

let fname = Sys.argv.(1) in (*debugger fait avec l'aide du prof, merci a lui*)
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