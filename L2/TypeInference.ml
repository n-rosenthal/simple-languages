(**
  Inferência Estática de Tipos para L2.
  Definição de um ambiente de tipos `env` sobre `name_bindings` (identificador, termo, tipo) e de um esquema de regras para o inferidor de tipos, no qual uma `rule` é um par de strings, onde a primeira representa o nome da regra e a segunda representa a regra concreta em si. `rules`, portanto, é um `rule list` ou `string list` explicitamente.

  Não existem exceções para inferência estática de tipos; portanto, o sistema de tipos é extendido para incluir `TypeError`. O inferidor retornará `TypeError` para todo termo mal-tipado.
  
  version 1.0
*)

open Terms
open Types

(**
  ambiente de tipos para L2
  
  um ambiente `env` é uma lista de `name_binding`, onde
  um `name_binding` é uma 3-upla
    (identificador: string, termo: term, tipo: tipo)
*)
type env = name_binding list
and name_binding = (string * term * tipo)
and lookup = string -> env -> (term * tipo) option
and update = string -> (term * tipo) -> env -> env
and insert = string -> (term * tipo) -> env -> env
;;

(* retorna um par (termo * tipo) dado um identificador `x` e um ambiente de tipos `env`, se `x` estiver no ambiente de tipos `env` *)
let rec lookup (x: string) (envtypes: env) : (term * tipo)  option = (
  match envtypes with
  | [] -> None
  | (y, e, t)::tl -> if x = y then Some (e, t) else lookup x tl
);;

(* insere um par (termo * tipo) no ambiente de tipos `env` *)
let insert (x: string) (e: term) (t: tipo) (envtypes: env) : env = (
  (x, e, t) :: envtypes
);;

(* atualiza um par (termo * tipo) no ambiente de tipos `env` *)
let update (x: string) (e: term) (t: tipo) (envtypes: env) : env = (
  List.map (fun (y, e', t') -> if x = y then (x, e, t) else (y, e', t')) envtypes
);;


(* repr. string de uma name_binding *)
let string_of_name_binding (x, e, t) : string = (
  "(" ^ x ^ ", " ^ string_of_term e ^ ", " ^ string_of_tipo t ^ ")"
);;

(* repr. string de um ambiente de tipos *)
let string_of_env (env: env) : string = (
  let rec aux (env: env) : string = (
    match env with
    | [] -> "Γ"
    | nb::tl -> "(" ^ string_of_name_binding nb ^ ") :: " ^ aux tl
  )
  in (
    "[" ^ aux env ^ "]"
  )
);;

(* esquema de regras para o inferidor de tipos *)
type rules = rule list
and rule = string * string
;;

let print_rules (s: string) (rules: rules) : unit = (
  print_endline s;
  List.iter (fun (r, e) -> print_endline (r ^ " : " ^ e)) rules
);;


(*  extensão sobre o sistema de tipos: um termo é tipado em `TypeError` (sic) sse ele for mal-tipado *)
type tipo +=
  | TypeError of string
;;

(** Inferência Estática de Tipos para L2 *
  Dado uma expressão (`term`) `e` e um ambiente de tipos (`env`), retorna o seu tipo (`tipo`), o ambiente de tipos atualizado (`env'`) e a lista de regras usadas (`rules`) para inferência de tipos estática de `e`
  
  Parameters
  -------
  e : term 
  
  env: env 
  
  Returns
  ------
  (tipo * env * rules)
*)
let rec typeinfer (e: term) (env: env) : (tipo * env * rules) = (match e with
  (**  valores   *)
  (*   Nothing (Unit, VoidType) *)
  | Nothing -> (Unit, env, [("T-Unit", string_of_env env ^ " ⊢ () : unit")])

  (*   Integer (Int n) *)
  | Integer n -> (Int, env, [("T-Int", string_of_env env ^ " ⊢ " ^ ast_of_term e ^ " : int")])

  (*   Boolean (Bool b) *)
  | Boolean b -> (Bool, env, [("T-Bool", string_of_env env ^ " ⊢ " ^ ast_of_term e ^ " : bool")])
  

  (*  Identificadores, var x *)
  | Identifier x -> (
    match lookup x env with
    | Some (e, t) -> (t, env, [("T-Var", string_of_env env ^ " ⊢ " ^ x ^ " : " ^ string_of_tipo t)])
    | None -> (TypeError "identificador não encontrado", env, [("T-Error UnboundIdentifier", x ^ " não encontrado")])
  )

  (*  Definição de identificador, let x : T = e1 in e2 *)
  | VarDefinition (x, e1, t, e2) -> (match typeinfer e1 env with
    | (t1, env', rules') when t1 = t -> (
      let env'' = insert x e1 t env' in
      let (t2, env''', rules'') = typeinfer e2 env'' in
      (t2, env''', rules' @ rules'' @ [("T-Let", string_of_env env ^ " ⊢ let " ^ x ^ " : " ^ string_of_tipo t ^ " = " ^ string_of_term e1 ^ " in " ^ string_of_term e2 ^ " : " ^ string_of_tipo t2)])
    )
    | _ -> (TypeError "tipos diferentes", env, [("T-Error DifferentTypes", "tipos diferentes")])
  )

  (*  if e1 then e2 else e3 *)
  | Conditional (e1, e2, e3) -> (match typeinfer e1 env with
    | (Bool, env', rules') -> (match (typeinfer e2 env', typeinfer e3 env') with
      | ((t2, env'', rules''), (t3, env''', rules''')) when t2 = t3 -> (t2, env''', rules' @ rules'' @ rules''' @ [("T-If", string_of_env env ^ " ⊢ if " ^ string_of_term e1 ^ " then " ^ string_of_term e2 ^ " else " ^ string_of_term e3 ^ " : " ^ string_of_tipo t2)])
      | _ -> (TypeError "tipos diferentes", env, rules' @ [("T-Error DifferentTypes", "tipos diferentes")])
    )
    | _ -> (TypeError "a condição e1 de um if(e1,e2,e3) deve ser tipada booleana", env, [("T-Error If TypeMismatch1", "a condição e1 de um if(e1,e2,e3) deve ser tipada booleana")]))

  | _ -> (TypeError "não implementado", env, [("T-Error NotImplemented", "não implementado: " ^ ast_of_term e ^ "\t" ^ string_of_term e)])
)
and typeof (e: term) : tipo = (match typeinfer e [] with
  | (t, _, _) -> t
  | _ -> TypeError "indefinido"
);;