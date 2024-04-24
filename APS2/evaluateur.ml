open Ast

(* Gestion des adresses mémoire : adresse suivante disponible et adresse maximale *)
let next_address = ref 0
let max_address = 10000


(* Extension des types pour supporter la gestion des tableaux et blocs mémoire dans APS2 *)
type address = int
and memory = (address, value) Hashtbl.t
and output = value list
and env = (string, value) Hashtbl.t
and value = 
  | InZ of int 
  | InF of singleExpr * string list * env 
  | InFR of singleExpr * string * string list * env 
  | InB of address * int
  | InA of address
  | InP of cmds * argsProc * env
  | InPR of cmds * string * argsProc * env 
  | Closure of cmds * string list * env

(* Vérification des opérateurs primitifs, incluant les opérations sur les tableaux pour APS2 *)
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

(* Fonction utilitaire pour générer une plage d'entiers, utilisée dans l'allocation de blocs mémoire *)
let range a b =
  let rec aux a b acc =
    if a >= b then acc
    else aux a (b-1) (b-1 :: acc)
  in aux a b []

(** Allocation d'une adresse mémoire unique, vérifie la disponibilité dans la limite maximale *)
let alloc mem =
  if !next_address >= max_address then
    failwith "Out of memory"
  else (
    let addr = !next_address in
    incr next_address; 
    Hashtbl.add mem addr (InZ 0);  
    addr
  )

(** Allocation d'un bloc de mémoire de taille n, important pour la gestion des tableaux dans APS2 *)
let allocn sigma n =
  let rec find_space addr =
    if addr + n > max_address then
      failwith "Memory full"
    else if List.for_all (fun i -> not (Hashtbl.mem sigma (addr + i))) (range 0 n)
    then addr
    else find_space (addr + 1)
  in
  let start_addr = find_space !next_address in
  for i = 0 to n - 1 do
    Hashtbl.add sigma (start_addr + i) (InZ 0) 
  done;
  next_address := start_addr + n;  
  (start_addr, sigma)


let nth_vector_element vector_base index mem =
  let actual_address = vector_base + index in
  match Hashtbl.find_opt mem actual_address with
  | Some value -> value
  | None -> failwith "Index out of bounds or not allocated"
  
let string_of_value = function
  | InZ n -> string_of_int n
  | InF _ -> "<function>"
  | InFR _ -> "<function_rec>"
  | InB (addr, size) -> Printf.sprintf "<block:%d size:%d>" addr size
  | InA addr -> Printf.sprintf "<address:%d>" addr
  | InP _ -> "<procedure>"
  | InPR _ -> "<procedure_rec>"
  | Closure _ -> "<closure>"

let string_of_output output =
  List.map string_of_value output |> String.concat " "

type output_stream = int list

(* Évaluation des opérations primitives incluant la gestion des tableaux dans APS2 *)
let eval_prim op args = 
  match op, args with
  | ASTId("not"), [InZ n] -> InZ (if n = 0 then 1 else 0)
  | ASTId("eq"), [InZ n1; InZ n2] -> InZ (if n1 = n2 then 1 else 0)
  | ASTId("lt"), [InZ n1; InZ n2] -> InZ (if n1 < n2 then 1 else 0)
  | ASTId("add"), [InZ n1; InZ n2] -> InZ (n1 + n2)
  | ASTId("sub"), [InZ n1; InZ n2] -> InZ (n1 - n2)
  | ASTId("mul"), [InZ n1; InZ n2] -> InZ (n1 * n2)
  | ASTId("div"), [InZ n1; InZ n2] -> InZ (n1 / n2)
  | ASTId("len"), [InB (_, size)] -> InZ size
  | ASTId("and"), [InZ n1; InZ n2] -> InZ (if n1 != 0 && n2 != 0 then 1 else 0)
  | _ ->
    let op_str = match op with
                 | ASTId(s) -> s
                 | _ -> failwith "Expected an operator as an identifier"
    in
    failwith ("Unsupported operator or arguments: " ^ op_str)
  
(* Conversion des booléens en valeurs entières *)
let eval_bool op = 
  match op with
  | "true" -> InZ 1
  | "false" -> InZ 0
  | _ -> failwith ("Unsupported bool operator: " ^ op)

(* Évaluation des expressions avec gestion des tableaux et effets sur la mémoire selon APS2 *)
let rec eval_expr (rho : env) (sigma : memory) (expr : singleExpr) : value * memory =
  match expr with
  | ASTId id when boolOp id -> (eval_bool id, sigma)
  | ASTId id ->
      (match Hashtbl.find_opt rho id with
       | Some (InA addr) ->  
           (match Hashtbl.find_opt sigma addr with
            | Some value -> (value, sigma)
            | None -> failwith ("Address " ^ string_of_int addr ^ " not found in memory"))
       | Some value -> (value, sigma)
       | None -> failwith (id ^ " : Variable or function not defined in the environment"))
  | ASTNum n -> (InZ n, sigma)
  | ASTAnd (e1, e2) ->
    let (v1, sigma1) = eval_expr rho sigma e1 in
    let (v2, sigma2) = eval_expr rho sigma1 e2 in
    (match v1, v2 with
    | InZ 1, InZ 1 -> (InZ 1, sigma2)
    | InZ 0, _ | _, InZ 0 -> (InZ 0, sigma2)
    | _ -> failwith "Non-integer operands for 'and'")
  | ASTOr (e1, e2) ->
    let (v1, sigma1) = eval_expr rho sigma e1 in
    let (v2, sigma2) = eval_expr rho sigma1 e2 in
    (match v1, v2 with
     | InZ 1, _ | _, InZ 1 -> (InZ 1, sigma2)  
     | InZ 0, InZ 0 -> (InZ 0, sigma2)    
     | _ -> failwith "Non-integer operands for 'or'")
  | ASTIf (cond, e_true, e_false) ->
    let (v_cond, sigma_cond) = eval_expr rho sigma cond in
    (match v_cond with
     | InZ 1 -> eval_expr rho sigma_cond e_true  
     | InZ 0 -> eval_expr rho sigma_cond e_false 
     | _ -> failwith "Non-boolean condition in 'if'")
  | ASTLambdaExpression (args, body) ->
    let xs = List.map (fun (ASTSingleArg (x, _)) -> x) args in
    (InF (body, xs, rho), sigma)
  | ASTApp (f, args) ->
    let evaluated_args = List.map (fun arg -> eval_expr rho sigma arg) args in
    let args_values, updated_sigma_list = List.split evaluated_args in
    let updated_sigma = List.fold_left (fun acc_sigma new_sigma -> new_sigma) sigma updated_sigma_list in
    (match f with
     | ASTId id when primOpCheck (ASTId id) ->
         (eval_prim (ASTId id) args_values, updated_sigma)
     | ASTId id ->
         (match Hashtbl.find_opt rho id with
          | Some InF (body, params, closure_env) ->
              let new_rho = List.fold_left2 (fun acc_env param arg_value ->
                  Hashtbl.add acc_env param arg_value; acc_env)
                  (Hashtbl.copy closure_env) params args_values in
              eval_expr new_rho updated_sigma body
          | Some InFR (body, func_name, params, closure_env) ->
              let self_ref = InFR (body, func_name, params, closure_env) in
              let new_rho = List.fold_left2 (fun acc_env param arg_value ->
                  Hashtbl.add acc_env param arg_value; acc_env)
                  (Hashtbl.copy closure_env) params args_values in
              Hashtbl.add new_rho func_name self_ref;
              eval_expr new_rho updated_sigma body
          | Some _ -> failwith ("Unsupported value type encountered for identifier: " ^ id)
          | None -> failwith ("The function " ^ id ^ " is not defined in the environment."))
     | ASTLambdaExpression(args_lambda, body_lambda) ->
         let params = List.map (fun (ASTSingleArg (x, _)) -> x) args_lambda in
         let new_rho = List.fold_left2 (fun acc_env param arg_value ->
             Hashtbl.add acc_env param arg_value; acc_env)
             (Hashtbl.copy rho) params args_values in
         eval_expr new_rho updated_sigma body_lambda
     | _ -> failwith "Function calls with complex expressions as functions not supported")
  | ASTAlloc e ->
      let (value, updated_sigma) = eval_expr rho sigma e in
      (match value with
      | InZ n ->
          let (addr, new_sigma) = allocn updated_sigma n in
          (InB (addr, n), new_sigma)
      | _ -> failwith "ASTAlloc expected an integer result")
  | ASTVset (e1, e2, e3) ->
    begin
      let (vec, sigma1) = eval_expr rho sigma e1 in
      let (index, sigma2) = eval_expr rho sigma1 e2 in
      let (new_val, sigma3) = eval_expr rho sigma2 e3 in
      Printf.printf "Setting vector at index %d to %d\n" (match index with InZ i -> i | _ -> -1) (match new_val with InZ n -> n | _ -> -1);
      match vec, index with
      | InB (base_addr, size), InZ idx when idx >= 0 && idx < size ->
          Hashtbl.replace sigma3 (base_addr + idx) new_val;
          Printf.printf "Successfully set\n";
          (InZ 0, sigma3)
      | _ -> failwith "ASTVset: Invalid vector or index"
    end
  | ASTLen e ->
    let (vec_val, sigma_after_vec) = eval_expr rho sigma e in
    (match vec_val with
     | InB (_, size) -> (InZ size, sigma_after_vec)
     | _ -> failwith "ASTLen: Argument is not a vector")
  | ASTNth (vec_expr, idx_expr) ->
    let (vec_val, sigma1) = eval_expr rho sigma vec_expr in
    match vec_val with
    | InB (base_addr, _) ->
      let (idx_val, sigma2) = eval_expr rho sigma1 idx_expr in
      (match idx_val with
      | InZ index -> (nth_vector_element base_addr index sigma2, sigma2)
      | _ -> failwith "Index must be an integer")
    | _ -> failwith "First argument must be a vector"

(* Évaluation des expressions dans les appels de procédures, avec support pour les tableaux d'APS2 *)
and eval_exprProc (rho : env) (sigma : memory) (exprProc : exprProc) : value =
  match exprProc with
  | ASTExpr e ->
      let (value, _) = eval_expr rho sigma e in
      value
  | ASTExprProcAdr id ->
      match Hashtbl.find_opt rho id with
      | Some (InA addr) -> InA addr
      | _ -> failwith (id ^ " : Expected a variable address for reference passing")
      
(* Convertir une lvalue en sa représentation chaîne de caractères. supporte les identifiants simples et les éléments de tableau, facilitant la gestion des affectations dans APS2 *)
and eval_lvalue_to_string lvalue =
  match lvalue with
  | ASTLValueIdent s -> s
  | ASTVectorValue (lv, _) -> eval_lvalue_to_string lv  

and eval_lvalue_to_expr (lval: lValue) (rho: env) (sigma: memory) : value * memory =
    match lval with
    | ASTLValueIdent id ->
        (match Hashtbl.find_opt rho id with
         | Some (InA addr) ->
             (match Hashtbl.find_opt sigma addr with
              | Some value -> (value, sigma)
              | None -> failwith ("Address not found in memory for variable " ^ id))
         | Some value -> (value, sigma)
         | None -> failwith ("Variable not defined in the environment: " ^ id))
    | ASTVectorValue (base_lval, index_expr) ->
        let (base, sigma1) = eval_lvalue_to_expr base_lval rho sigma in
        let (index, sigma2) = eval_expr rho sigma1 index_expr in
        match base, index with
        | InB (base_addr, _), InZ idx ->
            (match Hashtbl.find_opt sigma2 (base_addr + idx) with
             | Some value -> (value, sigma2)
             | None -> failwith "Index out of bounds or not allocated for vector")
        | _ -> failwith "Invalid base type for vector indexing"

(* Traitement des instructions avec mise à jour pour les affectations et manipulations de tableaux dans APS2 *)
and eval_stat (rho : env) (sigma : memory) (omega : output) (stat : stat) : memory * output =
  match stat with
  | ASTEcho expr ->
      let (result, updated_sigma) = eval_expr rho sigma expr in
      let new_output = result :: omega in 
      (updated_sigma, new_output)
  | ASTSet (lval, expr) ->
    begin
      match lval with
      | ASTLValueIdent id ->
          let (value, updated_sigma) = eval_expr rho sigma expr in
          (match Hashtbl.find_opt rho id with
          | Some (InA addr) ->
              Hashtbl.replace sigma addr value;  
              (updated_sigma, omega)
          | Some _ -> failwith (Printf.sprintf "Attempted to assign to a non-variable: %s" id)
          | None ->
              let addr = alloc sigma in 
              Hashtbl.add rho id (InA addr);
              Hashtbl.replace sigma addr value;
              (sigma, omega))
      | ASTVectorValue (base_lval, index_expr) ->
          let (base, base_sigma) = eval_lvalue_to_expr base_lval rho sigma in
          let (index, index_sigma) = eval_expr rho base_sigma index_expr in
          let (value, final_sigma) = eval_expr rho index_sigma expr in
          match base, index with
          | InB (base_addr, size), InZ idx ->
              if idx >= 0 && idx < size then
                begin
                  Hashtbl.replace final_sigma (base_addr + idx) value;  
                  (final_sigma, omega)
                end
              else
                failwith (Printf.sprintf "Vector index out of bounds: %d not in [0, %d)" idx size)
          | _, InZ idx -> failwith (Printf.sprintf "Invalid vector access at index %d" idx)
          | _ -> failwith "Invalid vector base or index type"
    end
  | ASTCall (proc_name, exprsProc) ->
    (match Hashtbl.find_opt rho proc_name with
    | Some (InP (cmds, proc_args, proc_env)) | Some (InPR (cmds, _, proc_args, proc_env)) ->
        let evaluate_arg (proc_arg: singleArgProc) (exprProc: exprProc) : string * value = match proc_arg, exprProc with
          | ASTSingleArgProcVar(name, _), ASTExprProcAdr id ->
            (match Hashtbl.find_opt rho id with
             | Some (InA addr) -> (name, InA addr)
             | _ -> failwith (id ^ " : Expected a variable address for reference passing"))
          | ASTSingleArgProc(name, _), ASTExpr e ->
            let (value, _) = eval_expr rho sigma e in
            (name, value)
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
  | ASTWhile (cond, block) ->
    let rec loop sigma' omega' =  
      let (condValue, sigma'') = eval_expr rho sigma' cond in
      match condValue with
      | InZ 0 -> (sigma'', omega')
      | InZ _ ->
          let (_, omega_loop) = eval_block rho sigma'' omega' block in  
          loop sigma'' omega_loop 
      | _ -> failwith "Condition of WHILE not boolean"
    in
    let (final_sigma, final_omega) = loop sigma omega in  
    (final_sigma, final_omega) 
  | ASTIf (e1, e2, e3) ->
    let (condValue, sigma1) = eval_expr rho sigma e1 in
    match condValue with
    | InZ 1 -> eval_block rho sigma1 omega e2  
    | InZ 0 -> eval_block rho sigma1 omega e3 
    | _ -> failwith "Non-integer condition in 'if'"

(* Définitions avec effet sur la mémoire pour les tableaux et structures modifiables d'APS2 *)
and eval_def (rho : env) (sigma : memory) (definition : def) : env * memory =
  match definition with
  | ASTConst (x, _, expr) ->
    let (result, updated_sigma) = eval_expr rho sigma expr in
    Hashtbl.add rho x result;  
    (rho, updated_sigma)
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

(* Évaluation récursive des commandes et blocs avec gestion étendue des tableaux et mémoire dans APS2 *)
and eval_cmds cmds env sigma omega = 
  match cmds with
  | ASTStat stat ->
    let updated_sigma, updated_omega = eval_stat env sigma omega stat in
    updated_sigma, updated_omega
  | ASTDef (def, more_cmds) ->
      let new_env, new_sigma = eval_def env sigma def in
      eval_cmds more_cmds new_env new_sigma omega
  | ASTStatWithCmds (stat, cmds) ->
      let updated_sigma, updated_omega = eval_stat env sigma omega stat in
      eval_cmds cmds env updated_sigma updated_omega

and eval_block (rho : env) (sigma : memory) (omega : output) (block : Ast.block) : memory * output =
  match block with
  | ASTBlock cmds -> eval_cmds cmds rho sigma omega

let eval_prog (p : Ast.block) =
  let rho_init = Hashtbl.create 100 in
  let sigma_init = Hashtbl.create 100 in
  let omega_init = [] in
  let _, final_output = eval_block rho_init sigma_init omega_init p in
  let output_in_correct_order = List.rev final_output in
  Printf.printf "%s" (string_of_output output_in_correct_order)
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
  Printf.printf ""
with
| Lexer.Eof -> Printf.printf "Error: Unexpected end of file.\n"