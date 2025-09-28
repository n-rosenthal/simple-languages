(*
  Sistema de tipos e inferência estática de tipos para L1
*)

(*  tipos de L1 *)
type tipo =
  | NoneType
  | Bool
  | Int
  | Pair of tipo * tipo
  | Exception of string         (*  Error é o tipo de um termo Exception x (equiv. valor Error x)*)
;;

(* repr. string de um tipo *)
let rec string_of_tipo (t: tipo) : string = match t with
  | NoneType -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | Pair (t1, t2) -> "(" ^ string_of_tipo t1 ^ " * " ^ string_of_tipo t2 ^ ")"
  | Exception s -> "Error " ^ s
;;


(*  ambiente de tipos: associa a cada NameBinding um tipo *)
type env = (Terms.binding * tipo) list;;


(*  busca por um identificador `x` no ambiente de tipos Γ e retorna o seu tipo `t`, se existir *)
let rec lookup (x: string) (envtypes: env) : (Terms.term * tipo)  option = (
  match envtypes with
  | [] -> None
  | ((e, y),  t)::tl -> if x = y then Some (e, t) else lookup x tl
)


(*  repr. string de um ambiente de tipos Γ 
  "[(x, e, t) :: (y, e, t) :: Γ]" etc
*)
let string_of_env (env: env) : string = (
  let rec aux (env: env) : string = (
    match env with
    | [] -> "Γ"
    | ((e, y), t)::tl -> "(" ^ y ^ ", " ^ Terms.string_of_term e ^ ", " ^ string_of_tipo t ^ ") :: " ^ aux tl
  )
  in (
    "[" ^ aux env ^ "]"
  )
);;

(*  TypeError ::= termo mal-formado ou de tipo indefinido *)
exception TypeError of string * Terms.term option;;

(* repr. string de um erro de tipagem *)
let string_of_exn (error: exn) : string = match error with
  | TypeError (s, t) -> s ^ (match t with
    | None -> ""
    | Some t -> ": " ^ Terms.string_of_term t)
  | _ -> raise error
;;

(*  dado um termo `e`, infere seu tipo `⊢ e : T`, armazenando as regras utilizidadas.
    retorna um resultado Ok (T, regras) ou Error (exceção) *)
    let rec typeinfer (e: Terms.term) (env: env) : (tipo * env * Repr.schema list) = (match e with
      (* Unit ()*)
      | Terms.None -> (NoneType, env, [])

      (* Integer n *)
      | Terms.Integer _ -> (Int, env, [("T-Int", [Terms.string_of_term e])])

      (* Boolean b *)
      | Terms.Boolean _ -> (Bool, env, [("T-Bool", [Terms.string_of_term e])])

      (* (e1, e2) *)
      | Terms.OrderedPair (e1, e2) -> let (t1, env1, rules1) = typeinfer e1 env in
                                      let (t2, env2, rules2) = typeinfer e2 env1 in
                                      (Pair (t1, t2), env2, rules1 @ rules2 @ [("T-OrderedPair", [Terms.string_of_term e1; string_of_tipo t1; Terms.string_of_term e2; string_of_tipo t2; Terms.string_of_term e])])

      (* fst e *)
      | Terms.Fst e -> (match (typeinfer e env) with
        | (Pair (t1, t2), env1, rules) -> (t1, env1, rules @ [("T-Fst", [Terms.string_of_term e; Terms.string_of_term e])])
        | _ -> (Exception "TypeError: argument of `fst e` must be a pair", env, [("T-Fst", [Terms.string_of_term e; Terms.string_of_term e])])
      )

      (* snd e *)
      | Terms.Snd e -> (match (typeinfer e env) with
        | (Pair (t1, t2), env1, rules) -> (t2, env1, rules @ [("T-Snd", [Terms.string_of_term e; Terms.string_of_term e])])
        | _ -> (Exception "TypeError: argument of `snd e` must be a pair", env, [("T-Snd", [Terms.string_of_term e; Terms.string_of_term e])])
      )

      (* if e1 then e2 else e3 *)
      | Terms.Conditional (e1, e2, e3) -> (match (typeinfer e1 env) with
        | (Bool, env1, rules1) -> (
          let (t2, env2, rules2) = typeinfer e2 env1 in
          let (t3, env3, rules3) = typeinfer e3 env2 in
          (match (t2, t3) with
            | (t2, t3) when t2 = t3 -> (t2, env3, rules1 @ rules2 @ rules3 @ [("T-If", [Terms.string_of_term e1; Terms.string_of_term e2; Terms.string_of_term e3])])
            | _ -> ((Exception ("TypeError: branches of `if e1 then e2 else e3` must have the same type, got `e2: " ^ string_of_tipo t2 ^ "` and `e3: " ^ string_of_tipo t3 ^ "`")), env, [("T-If", [Terms.string_of_term e1; Terms.string_of_term e2; Terms.string_of_term e3])])
          )
        )
        | _ -> (Exception ("TypeError: first argument of `if e1 then e2 else e3` must be a boolean, got `" ^ Terms.string_of_ast e1 ^ "`"), env, [("T-If", [Terms.string_of_term e1; Terms.string_of_term e2; Terms.string_of_term e3])])
      )

      (* x *)
      | Terms.Identifier x -> (match (lookup x env) with
        | Some (e1, t1) -> (t1, env, [("T-Var", [Terms.string_of_term e1; Terms.string_of_term e])])
        | None -> (Exception ("unbound variable `" ^ x ^ "`"), env, [("T-Var", [Terms.string_of_term e; Terms.string_of_term e])])
      )

      (* let x = e1 in e2 *)
      | Terms.VarDefinition (nb, e2) -> (match nb with
        | (e1, x) -> (
            let (t1, env', rules') = typeinfer e1 env in 
            let env'' = (nb, t1) :: env' in
            let (t2, env'', rules'') = typeinfer e2 env'' in
            (t2, env'', rules' @ rules'' @ [("T-VarDefinition", [Terms.string_of_term e1; Terms.string_of_term e2; Terms.string_of_term e])]))
        | _ -> raise (TypeError ("cannot typeinfer `" ^ Terms.string_of_term e ^ "`", Some e))
      )

    | _ -> raise (TypeError ("cannot typeinfer `" ^ Terms.string_of_term e ^ "`", Some e))
    )
;;
