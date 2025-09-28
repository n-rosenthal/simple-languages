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
    | (e, v, x)::tl -> "(" ^ Terms.string_of_ast e ^ ", " ^ Terms.string_of_value v ^ ", " ^ x ^ ") " ^ (aux tl)
  in aux env
;; 

let rec eval (e: Terms.term) (env: env) (types: Types.env) : (Terms.value * env) = (
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
    | _ -> raise (RuntimeError ("cannot evaluate `" ^ Terms.string_of_term e ^ "`")))

    (* snd e *)
  | Terms.Snd e -> (match (eval e env types) with
    | (Terms.VPair (v1, v2), env1) -> (v2, env1)
    | _ -> raise (RuntimeError ("cannot evaluate `" ^ Terms.string_of_term e ^ "`")))
  
  (* if e1 then e2 else e3 *)
  | Terms.Conditional (e1, e2, e3) -> (match (eval e1 env types) with
    | (Terms.VBool b, env1) -> if b then (eval e2 env1 types) else (eval e3 env1 types)
    | _ -> raise (RuntimeError ("cannot evaluate `" ^ Terms.string_of_term e ^ "`")))

  (* let x = e1 in e2 *)
  | Terms.VarDefinition (nb, e2) -> (
      let (e1, x) = nb in (
        match (Types.typeinfer e1 types) with
        | Ok (t1, types', _) -> (
          match (eval e1 env types') with
          | (v1, env1) -> (
            match (eval e2 ((e1, v1, x)::env1) types') with
            | (v2, env2) -> (v2, env2)
            | _ -> raise (RuntimeError ("cannot evaluate `" ^ Terms.string_of_term e ^ "`"))
          )
          | _ -> raise (RuntimeError ("cannot evaluate `" ^ Terms.string_of_term e ^ "`"))
        )
        | Error exn -> raise exn
        )
    )

    (* x *)
  | Terms.Identifier x -> (match (lookup x env) with
    | Some (e1, v1, x1) -> (v1, env)
    | None -> raise (RuntimeError ("Unbound identifier `" ^ x ^ "`")))

| _ -> raise (RuntimeError ("cannot evaluate `" ^ Terms.string_of_term e ^ "`"))
);;