open Ast

module Env = Map.Make(String)

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

(* Définition des types de valeurs manipulées dans APS0 : entiers, fonctions, et fonctions récursives *)
type value = 
  InZ of int
  | InF of singleExpr * string list * value Env.t 
  | InFR of singleExpr * string * string list * value Env.t 

(* Affichage des valeurs entières, seule instruction d'effet de bord en APS0 *)
let print_value value = (* <-- fait avec l'aide d'un camarade dans la salle TME (Yanis & Salim Tabellout )*)
  match value with
    InZ(n) -> Printf.printf "%d\n" n
  | _ -> failwith ("Type non entier")

type output_stream = int list

(* Extraction des identifiants des arguments pour la manipulation des fonctions *)
let get_arg_ident (arg) =   (* <-- fait avec l'aide d'un camarade dans la salle TME (Yanis & Salim Tabellout )*)
  match arg with 
  ASTSingleArg (ident,_) -> ident 
  

let rec get_args_in_string_list (argz) : (string list) =   (* <-- fait avec l'aide d'un camarade dans la salle TME (Yanis & Salim Tabellout )*)
  match argz with 
  |  [] -> []
  |  a::argz2 -> 
      (get_arg_ident a)::(get_args_in_string_list argz2)

(* Évaluation des opérations primitives sur les entiers selon APS0 *)
let eval_prim op args = 
  match op, args with
  | ASTId("not"), [InZ n] -> InZ (if n = 0 then 1 else 0)
  | ASTId("eq"), [InZ n1; InZ n2] -> InZ (if n1 = n2 then 1 else 0)
  | ASTId("lt"), [InZ n1; InZ n2] -> InZ (if n1 < n2 then 1 else 0)
  | ASTId("add"), [InZ n1; InZ n2] -> InZ (n1 + n2)
  | ASTId("sub"), [InZ n1; InZ n2] -> InZ (n1 - n2)
  | ASTId("mul"), [InZ n1; InZ n2] -> InZ (n1 * n2)
  | ASTId("div"), [InZ n1; InZ n2] -> InZ (n1 / n2)
  | _ -> failwith ("Opérateur ou arguments non pris en charge")

(* Évaluation des opérateurs booléens 'true' et 'false' *)
let eval_bool op  = 
  match op with
  | "true" -> InZ 1
  | "false" -> InZ 0
  | _ -> failwith (" Opérateur bool non pris en charge")

(* Fonction d'évaluation des expressions APS0, incluant les opérations primitives et les appels de fonction *)
let rec eval_expr x env = 
  match x with 
  | ASTNum n -> InZ n 
  | ASTId id -> (match (boolOp id) with
    | true -> eval_bool id
    | false -> (match Env.find_opt id env with 
                | Some v -> v
                | None -> failwith (id^" : Variable non définie dans l'environnement")))
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
  | ASTLambdaExpression (args, body) -> 
    let xs = List.map (fun (ASTSingleArg (x,t))-> x) args in InF(body,xs,env)
  | ASTApp (func, args) -> 
    let args_values = List.map (fun arg -> eval_expr arg env) args in
    (match (primOpCheck func) with
        | true -> eval_prim func args_values
        | false -> let func_value = eval_expr func env in 
          (match func_value with
            | InF (body, params, env2) ->
                let new_env = List.fold_left2 (fun acc param arg_value -> Env.add param arg_value acc) env2 params args_values in
                eval_expr body new_env
            | InFR (body, func_name, params, env2) -> 
                let new_env = List.fold_left2 (fun acc param arg_value -> Env.add param arg_value acc) env2 params args_values in
                eval_expr body (Env.add func_name func_value new_env) 
            | _ -> failwith (" Impossible d'appeler une fonction qui n'est pas une fermeture")))

(* Évaluation de l'instruction 'echo', produisant une sortie dans APS0 *)
let rec eval_stat ins env flx = 
  match ins with
  | ASTEcho expr ->
      let result = eval_expr expr env in
      result :: flx

(* Évaluation des définitions de constantes et fonctions, mise à jour de l'environnement *)
let rec eval_def def env = 
  match def with
  | ASTConst (x, _, expr) ->
      let result = eval_expr expr env in
      Env.add x result env
  | ASTFun (x, _, args, expr) ->
    let args_string = get_args_in_string_list args in
    let closure = InF (expr, args_string, env) in
      Env.add x closure env
  | ASTFunRec (x, _, args, expr) ->
    let args_string = get_args_in_string_list args in
    let recursion = InFR (expr, x, args_string, env) in
    Env.add x recursion env

(* Évaluation des commandes dans un programme APS0 *)
let rec eval_cmds cmds env flx =
  match cmds with
      | ASTDef (def, more_cmds) ->
          let new_env = eval_def def env in
          eval_cmds more_cmds new_env flx
      | ASTStat stat ->
          eval_stat stat env flx  

(* Point d'entrée pour l'évaluation d'un programme APS0 *)
let rec eval_prog p =
  let final_output = eval_cmds p Env.empty [] in
  List.iter (function x -> print_value x) (List.rev final_output) 
;;

(* Lecture et évaluation du programme APS0 depuis un fichier d'entrée *)
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