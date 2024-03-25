open Ast

(*E(Env) = ident(key string) -> V(value)*)
module Env = Map.Make(String)
  (*update environement Env.add*)

(*-Utils-*)

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


(*-V = Z ⊕ F ⊕ FR-*)
type value = 
  InZ of int (*-Z = valeurs immediates*)
  | InF of singleExpr * string list * value Env.t (*-F(InF) = Expr(string) × ident∗(string list) × E(value Env.k)-*)
  | InFR of singleExpr * string * string list * value Env.t (*-FR(InFR) = Expr(string) × ident(string) × ident∗(string list) × E(value Env.k)-*)
  
let print_value value = 
  match value with
    InZ(n) -> Printf.printf "%d\n" n
  | _ -> failwith ("Type non entier")
  (*| _ -> failwith (value^" Type non entier")*)

(*-O(output_stream) = Z∗(int list)-*)
type output_stream = int list

let get_arg_ident (arg) =   (* <-- fait grace a l'aide d'un camarade dans la salle TME *)
  match arg with 
  ASTSingleArg (ident,_) -> ident 
  

let rec get_args_in_string_list (argz) : (string list) =   (* <-- fait grace a l'aide d'un camarade dans la salle TME *)
  match argz with 
  |  [] -> []
  |  a::argz2 -> 
      (get_arg_ident a)::(get_args_in_string_list argz2)

(*Fonctions primitives*)
let eval_prim op args = 
  match op, args with
  (* Opérateurs unaires *)
  | ASTId("not"), [InZ n] -> InZ (if n = 0 then 1 else 0)
  (* Opérateurs binaires *)
  | ASTId("eq"), [InZ n1; InZ n2] -> InZ (if n1 = n2 then 1 else 0)
  | ASTId("lt"), [InZ n1; InZ n2] -> InZ (if n1 < n2 then 1 else 0)
  | ASTId("add"), [InZ n1; InZ n2] -> InZ (n1 + n2)
  | ASTId("sub"), [InZ n1; InZ n2] -> InZ (n1 - n2)
  | ASTId("mul"), [InZ n1; InZ n2] -> InZ (n1 * n2)
  | ASTId("div"), [InZ n1; InZ n2] -> InZ (n1 / n2)
  | _ -> failwith ("Opérateur ou arguments non pris en charge")

  let eval_bool op  = 
  match op with
  | "true" -> InZ 1
  | "false" -> InZ 0
  | _ -> failwith (" Opérateur bool non pris en charge")

let rec eval_expr x env = 
  match x with 
  | ASTNum n -> InZ n 
  | ASTId id -> (match (boolOp id) with
    | true -> eval_bool id
    | false -> (match Env.find_opt id env with 
                | Some v -> v
                | None -> failwith (id^" : Variable non définie dans l'environnement")))
  (* Gestion des opérateurs logiques et de contrôle *)
  | ASTAnd (e1, e2) ->
    let v1 = eval_expr e1 env in
    let v2 = eval_expr e2 env in
    if v1 = InZ 1 && v2 = InZ 1 then InZ 1 else InZ 0
  | ASTOr (e1, e2) ->
      let v1 = eval_expr e1 env in
      let v2 = eval_expr e2 env in
      if v1 = InZ 1 || v2 = InZ 1 then InZ 1 else InZ 0
  | ASTIf (e1, e2, e3) ->
      let v1 = eval_expr e1 env in
      if v1 = InZ 1 then eval_expr e2 env else eval_expr e3 env
  (* Cas de l'abstraction de fonction *)
  | ASTLambdaExpression (args, body) -> 
    let xs = List.map (fun (ASTSingleArg (x,t))-> x) args in InF(body,xs,env)
  (* Cas de l'application de fonction *)
  | ASTApp (func, args) -> 
    let args_values = List.map (fun arg -> eval_expr arg env) args in
    (match (primOpCheck func) with
        | true -> eval_prim func args_values
        | false -> let func_value = eval_expr func env in 
          (match func_value with
            | InF (body, params, env2) ->
                let new_env = List.fold_left2 (fun acc param arg_value -> Env.add param arg_value acc) env2 params args_values in
                eval_expr body new_env
            | InFR (body, func_name, params, env2) -> (* Change here *)
                let new_env = List.fold_left2 (fun acc param arg_value -> Env.add param arg_value acc) env2 params args_values in
                eval_expr body (Env.add func_name func_value new_env) (* Pass the function name and its closure *)
            | _ -> failwith (" Impossible d'appeler une fonction qui n'est pas une fermeture")))

let rec eval_stat ins env flx = 
  match ins with
  (* Évaluation de l'instruction ECHO *)
  | ASTEcho expr ->
      let result = eval_expr expr env in
      (* Ajout du résultat à la sortie *)
      result :: flx

let rec eval_def def env = 
  match def with
  (* Évaluation de la définition CONST *)
  | ASTConst (x, _, expr) ->
      let result = eval_expr expr env in
      (* Mise à jour de l'environnement avec la valeur calculée *)
      Env.add x result env
  (* Évaluation de la définition FUN *)
  | ASTFun (x, _, args, expr) ->
    let args_string = get_args_in_string_list args in
    let closure = InF (expr, args_string, env) in
      (* Mise à jour de l'environnement avec la fermeture calculée *)
      Env.add x closure env
  (* Évaluation de la définition FUN REC *)
  | ASTFunRec (x, _, args, expr) ->
    let args_string = get_args_in_string_list args in
    let recursion = InFR (expr, x, args_string, env) in
    (* Mise à jour de l'environnement avec la fermeture récursive calculée *)
    Env.add x recursion env

let rec eval_cmds cmds env flx =
  match cmds with
      | ASTDef (def, more_cmds) ->
          let new_env = eval_def def env in
          eval_cmds more_cmds new_env flx
      | ASTStat stat ->
          eval_stat stat env flx  

let rec eval_prog p =
  let final_output = eval_cmds p Env.empty [] in
  List.iter (function x -> print_value x) (List.rev final_output) 

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
        Printf.printf "Erreur de syntaxe à la ligne %d, colonne %d\n" line cnum;
        exit 1
  in
  let _ = eval_prog p in
  Printf.printf "Evaluation terminée avec succès.\n"
with
| Lexer.Eof -> Printf.printf "Erreur : fin de fichier inattendue.\n"