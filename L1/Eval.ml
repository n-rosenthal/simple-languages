(*
  Avaliador small-step para L1
*)

(* ambiente de valores para L1 *)
type binding  = (Terms.term * Terms.value * string);; (* e, v, x*)
type env      = binding list;;

exception RuntimeError of string;;

let string_of_exn (exn: exn) : string = match exn with
  | RuntimeError s -> "RuntimeError: " ^ s
  | _ -> raise exn

let rec lookup (x: string) (env: env) : (Terms.term * Terms.value * string)  option = (
  match env with
  | [] -> None
  | (e, v, x)::tl -> if x = x then Some (e, v, x) else lookup x tl
);;

let string_of_env (env: env) : string =
  let rec aux (env: env) : string = match env with
    | [] -> "Δ"
    | (e, v, x)::tl -> "(" ^ Terms.ast_of_term e ^ ", " ^ Terms.string_of_value v ^ ", " ^ x ^ ") " ^ (aux tl)
  in aux env
;; 

let rec eval (e: Terms.term) (env: env) (types: Middleware.env) : (Terms.value * env) = (
  match e with
    (*  Unit () *)
  | Terms.None -> (Terms.Unit, env)

    (* Integer n *)
  | Terms.Integer n -> (Terms.VInt n, env)

    (* Boolean b *)
  | Terms.Boolean b -> (Terms.VBool b, env)

    (* (e1, e2) *)
  | Terms.OrderedPair (e1, e2) -> let (v1, env1) = eval e1 env types in
                                  let (v2, env2) = eval e2 env1 types in
                                  (Terms.VPair (v1, v2), env2)

    (* fst e *)
  | Terms.Fst e -> (match (eval e env types) with
    | (Terms.VPair (v1, v2), env1) -> (v1, env1)
    | _ -> (Error ("argument `e` of `fst e` must be a pair, got `" ^ Terms.ast_of_term e ^ "`"), env))

    (* snd e *)
  | Terms.Snd e -> (match (eval e env types) with
    | (Terms.VPair (v1, v2), env1) -> (v2, env1)
    | _ -> (Error ("argument `e` of `snd e` must be a pair, got `" ^ Terms.ast_of_term e ^ "`"), env))
  
  (* if e1 then e2 else e3 *)
  | Terms.Conditional (e1, e2, e3) -> (match (eval e1 env types) with
    | (Terms.VBool b, env1) -> if b then (eval e2 env1 types) else (eval e3 env1 types)
    | _ -> (Error ("argument `e1` of `if e1 then e2 else e3` must be a boolean, got `" ^ Terms.ast_of_term e1 ^ "`"), env))

  | _ -> raise (RuntimeError ("cannot evaluate `" ^ Terms.string_of_term e ^ "`"))
);;