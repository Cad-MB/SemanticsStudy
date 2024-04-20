open Ast

type address = int
type memory = (address, int) Hashtbl.t
let next_address = ref 0

let primOpCheck op = 
  match op with
  | ASTId("not") -> true
  | ASTId("add") -> true
  | ASTId("mul") -> true
  | ASTId("sub") -> true
  | ASTId("div") -> true
  | ASTId("eq") -> true
  | ASTId("lt") -> true
  | ASTId("true") -> true
  | ASTId("false") -> true
  | _ -> false

let boolOp op = 
  match op with
  | "true" -> true
  | "false" -> true
  | _ -> false

type output = value list
and env = (string, value) Hashtbl.t
and value = 
  | InZ of int 
  | InF of singleExpr * string list * env 
  | InFR of singleExpr * string * string list * env 
  | InA of address
  | InP of cmds * argsProc * env
  | InPR of cmds * string * argsProc * env 
  | Closure of cmds * string list * env

let alloc mem =
  let addr = !next_address in
  incr next_address;
  Hashtbl.add mem addr 0; 
  addr
  
let print_value value = 
  match value with
    InZ(n) -> Printf.printf "%d\n" n
  | _ -> failwith ("Non-integer type")

type output_stream = int list

let get_arg_ident (arg) =   
  match arg with 
  ASTSingleArg (ident,_) -> ident 
  
let rec get_args_in_string_list (argz) : (string list) =   
  match argz with 
  |  [] -> []
  |  a::argz2 -> 
      (get_arg_ident a)::(get_args_in_string_list argz2)

let eval_prim op args = 
  match op, args with
  | ASTId("not"), [InZ n] -> InZ (if n = 0 then 1 else 0)
  | ASTId("eq"), [InZ n1; InZ n2] -> InZ (if n1 = n2 then 1 else 0)
  | ASTId("lt"), [InZ n1; InZ n2] -> InZ (if n1 < n2 then 1 else 0)
  | ASTId("add"), [InZ n1; InZ n2] -> InZ (n1 + n2)
  | ASTId("sub"), [InZ n1; InZ n2] -> InZ (n1 - n2)
  | ASTId("mul"), [InZ n1; InZ n2] -> InZ (n1 * n2)
  | ASTId("div"), [InZ n1; InZ n2] -> InZ (n1 / n2)
  | _ -> failwith ("Unsupported operator or arguments")

let eval_bool op  = 
  match op with
  | "true" -> InZ 1
  | "false" -> InZ 0
  | _ -> failwith ("Unsupported bool operator")

let rec eval_expr (rho : env) (sigma : memory) (expr : singleExpr) : value =
  match expr with
  | ASTNum n -> InZ n
  | ASTId id ->
    if boolOp id then eval_bool id 
    else 
      (match Hashtbl.find_opt rho id with
       | Some (InA addr) ->  
         (match Hashtbl.find_opt sigma addr with
          | Some int_value -> InZ int_value
          | None -> failwith ("Adress " ^ string_of_int addr ^ " not found in memory"))
       | Some (InF (body, params, closure_env)) -> InF (body, params, closure_env) 
       | Some (InFR (body, func_name, params, closure_env)) -> InFR (body, func_name, params, closure_env)  
       | Some v -> v  
       | None -> failwith (id ^ " : Variable or function not defined in the environment"))
  | ASTAnd (e1, e2) ->
      let v1 = eval_expr rho sigma e1 in
      let v2 = eval_expr rho sigma e2 in
      (match v1, v2 with
       | InZ 1, InZ 1 -> InZ 1
       | InZ 0, _ | _, InZ 0 -> InZ 0
       | _ -> failwith "Non-integer operands for 'and'")
  | ASTOr (e1, e2) ->
      let v1 = eval_expr rho sigma e1 in
      let v2 = eval_expr rho sigma e2 in
      (match v1, v2 with
       | InZ 0, InZ 0 -> InZ 0
       | InZ 1, _ | _, InZ 1 -> InZ 1
       | _ -> failwith "Non-integer operands for 'or'")
  | ASTIf (e1, e2, e3) ->
      let v1 = eval_expr rho sigma e1 in
      (match v1 with
       | InZ 1 -> eval_expr rho sigma e2
       | InZ 0 -> eval_expr rho sigma e3
       | _ -> failwith "Non-integer operands for 'if'")
  | ASTLambdaExpression (args, body) ->
      let xs = List.map (fun (ASTSingleArg (x, _)) -> x) args in
      InF (body, xs, rho)
  | ASTApp (f, args) ->
  let args_values = List.map (fun arg -> eval_expr rho sigma arg) args in
  (match f with
   | ASTId id when primOpCheck (ASTId id) ->  
       eval_prim (ASTId id) args_values
   | ASTId id ->
       (match Hashtbl.find_opt rho id with
        | Some InF (body, params, closure_env) ->
          let new_rho = List.fold_left2 (fun acc_env param arg_value ->
              Hashtbl.add acc_env param arg_value; acc_env) 
              (Hashtbl.copy closure_env) params args_values in
          eval_expr new_rho sigma body
        | Some InFR (body, func_name, params, closure_env) ->
            let self_ref = InFR (body, func_name, params, closure_env) in
            let new_rho = List.fold_left2 (fun acc_env param arg_value ->
                Hashtbl.add acc_env param arg_value; acc_env) 
                (Hashtbl.copy closure_env) params args_values in
            Hashtbl.add new_rho func_name self_ref;
            eval_expr new_rho sigma body
        | Some _ -> failwith ("Unsupported value type encountered for identifier: " ^ id)
        | None -> failwith ("The function " ^ id ^ " is not defined in the environment.")
        )
   | ASTLambdaExpression(args_lambda, body_lambda) ->
       let params = List.map (fun (ASTSingleArg (x, _)) -> x) args_lambda in
       let new_rho = List.fold_left2 (fun acc_env param arg_value ->
           Hashtbl.add acc_env param arg_value; acc_env)
           (Hashtbl.copy rho) params args_values in
       eval_expr new_rho sigma body_lambda
   | _ -> failwith "Function calls with complex expressions as functions not supported")

and eval_exprProc (rho : env) (sigma : memory) (exprProc : exprProc) : value =
  match exprProc with
  | ASTExpr e -> eval_expr rho sigma e
  | ASTExprProcAdr id ->
      match Hashtbl.find_opt rho id with
      | Some (InA addr) -> InA addr
      | _ -> failwith (id ^ " : Expected a variable address for reference passing")
  
and eval_stat (rho : env) (sigma : memory) (omega : output) (instruction : stat) : memory * output =
  match instruction with
  | ASTEcho expr ->
      let result = eval_expr rho sigma expr in
      (sigma, result :: omega) 
  | ASTSet (id, expr) ->
      (match Hashtbl.find_opt rho id with
       | Some (InA addr) ->
           let value = eval_expr rho sigma expr in
           (match value with
            | InZ n -> 
              Hashtbl.replace sigma addr n;  
              (sigma, omega)  
            | _ -> failwith "Only integers can be stored in memory")
       | None ->
        Printf.printf "Attempting to SET on an undeclared variable : %s\n" id;
        Hashtbl.iter (fun key _ -> Printf.printf "Var in env: %s\n" key) rho;
        failwith "Variable not declared during assignment"
       | _ -> failwith "Variable not declared during assignment")
  | ASTIf (cond, block1, block2) ->
      let cond_value = eval_expr rho sigma cond in
      (match cond_value with
       | InZ 1 -> eval_block rho sigma omega block1
       | InZ 0 -> eval_block rho sigma omega block2
       | _ -> failwith "IF condition not evaluated to an integer")
  | ASTWhile (cond, block) ->
      let rec eval_while rho sigma omega =
        let cond_value = eval_expr rho sigma cond in
        match cond_value with
        | InZ 1 ->
            let (new_sigma, new_omega) = eval_block rho sigma omega block in
            eval_while rho new_sigma new_omega
        | InZ 0 -> (sigma, omega)
        | _ -> failwith "WHILE condition not evaluated to an integer"
      in
      eval_while rho sigma omega
  | ASTCall (proc_name, exprsProc) ->
    (match Hashtbl.find_opt rho proc_name with
    | Some (InP (cmds, proc_args, proc_env)) | Some (InPR (cmds, _, proc_args, proc_env)) ->
        let evaluate_arg (proc_arg: singleArgProc) (exprProc: exprProc) : string * value = match proc_arg, exprProc with
          | ASTSingleArgProcVar(name, _), ASTExprProcAdr id ->
            (match Hashtbl.find_opt rho id with
             | Some (InA addr) -> (name, InA addr)
             | _ -> failwith (id ^ " : Expected a variable address for reference passing"))
          | ASTSingleArgProc(name, _), ASTExpr e -> (name, eval_expr rho sigma e)
          | _ -> failwith "Mismatch between procedure argument type and provided argument"
        in
        let evaluated_args = List.map2 evaluate_arg proc_args exprsProc in
        let new_env = List.fold_left (fun acc_env (name, arg_value) ->
            Hashtbl.add acc_env name arg_value; acc_env)
            (Hashtbl.copy proc_env) evaluated_args
        in
        let _, new_omega = eval_block new_env sigma omega (ASTBlock cmds) in
        (sigma, new_omega)
    | _ -> failwith ("Procedure " ^ proc_name ^ " not found"))

and eval_def (rho : env) (sigma : memory) (definition : def) : env * memory =
  match definition with
  | ASTConst (x, _, expr) ->
    let result = eval_expr rho sigma expr in
    Hashtbl.add rho x result;
    (rho, sigma)
  | ASTVar (x, _) ->
    let addr = alloc sigma in 
    Hashtbl.add rho x (InA addr); 
    (rho, sigma)
  | ASTFun (x, _, args, body) ->
    let args_names = List.map (fun (ASTSingleArg (name, _)) -> name) args in
    let closure_env = Hashtbl.copy rho in
    Hashtbl.add rho x (InF (body, args_names, closure_env));
    (rho, sigma)
  | ASTFunRec (x, _, args, body) ->
    let args_names = List.map (fun (ASTSingleArg (name, _)) -> name) args in
    let placeholder_env = Hashtbl.copy rho in
    let temp_func_val = InFR (body, x, args_names, placeholder_env) in
    Hashtbl.add placeholder_env x temp_func_val; 
    Hashtbl.add rho x temp_func_val;
    (rho, sigma)
  | ASTProc (name, args, ASTBlock cmds) ->
    let proc_val = InP (cmds, args, rho) in
    Hashtbl.add rho name proc_val;
    (rho, sigma)
  | ASTProcRec (name, args, ASTBlock cmds) ->
    let new_env = Hashtbl.copy rho in
    let proc_val = InPR (cmds, name, args, new_env) in
    Hashtbl.add new_env name proc_val;
    Hashtbl.add rho name proc_val;
    (rho, sigma)

and eval_cmds cmds env sigma omega = 
  match cmds with
  | ASTDef (def, more_cmds) ->
      let new_env, new_sigma = eval_def env sigma def in
      eval_cmds more_cmds new_env new_sigma omega
  | ASTStat stat ->
      let updated_sigma, updated_omega = eval_stat env sigma omega stat in
      updated_sigma, updated_omega
  | ASTStatWithCmds (stat, cmds) ->
      let updated_sigma, updated_omega = eval_stat env sigma omega stat in
      eval_cmds cmds env updated_sigma updated_omega

and eval_block (rho : env) (sigma : memory) (omega : output) (block : Ast.block) : memory * output =
  match block with
  | ASTBlock cmds -> eval_cmds cmds rho sigma omega

  let rec eval_prog (p : Ast.block) =
    let rho_init = Hashtbl.create 100 in  
    let sigma_init = Hashtbl.create 100 in  
    let omega_init = [] in  
    let _, final_output = eval_block rho_init sigma_init omega_init p in
    List.iter print_value final_output  

;;

let fname = Sys.argv.(1) in
let ic = open_in fname in
try
  let lexbuf = Lexing.from_channel ic in
  let p =
    try
      Parser.prog Lexer.token lexbuf
    with
    | Parsing.Parse_error ->
        let open Lexing in
        let curr = lexbuf.lex_curr_p in
        let line = curr.pos_lnum in
        let cnum = curr.pos_cnum - curr.pos_bol in
        Printf.printf "Syntax error on line %d, column %d\n" line cnum;
        exit 1
  in
  let _ = eval_prog p in
  Printf.printf "Evaluation completed successfully.\n"
with
| Lexer.Eof -> Printf.printf "Error: Unexpected end of file.\n"