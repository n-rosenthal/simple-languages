open Middleware




(*  busca por um identificador `x` no ambiente de tipos Γ e retorna o seu tipo `t`, se existir *)
let rec lookup (x: string) (envtypes: env) : (Terms.term * Types.tipo)  option = (
  match envtypes with
  | [] -> None
  | ((e, y),  t)::tl -> if x = y then Some (e, t) else lookup x tl
)


(** Inferência estática de tipos sobre L1
    Dado uma expressão (`term`) `e` e um ambiente de tipos (`env`), retorna o seu tipo (`tipo`) e o ambiente de tipos atualizado (`env'`)
    
    @param e Terms.term
    @param env Types.env
    @returns Types.tipo * Types.env * Repr.schema list
    @raises TypeError
*)
let rec typeinfer (e: Terms.term) (env: env) : (Types.tipo * env * (string * string) list) = (match e with
  (** valores *)
  (* None *)
  | Terms.None -> (Types.NoneType, env, [("T-None", string_of_env env ^ " ⊢ (): unit")])

  (* Integer n *)
  | Terms.Integer n -> (Types.Int, env, [("T-Int", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": int")])

  (* Boolean b *)
  | Terms.Boolean b -> (Types.Bool, env, [("T-Bool", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": bool")])

  (* (e1, e2) *)
  | Terms.OrderedPair (e1, e2) -> (match typeinfer e1 env with
    | Types.ExceptionType exn, env', rules -> (Types.ExceptionType exn, env', rules @ [("(" ^ Types.string_of_error exn ^ ") || ExnPropag. (err 1)", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Types.string_of_error exn)])
    | t1, env1, rules1 -> (match typeinfer e2 env1 with
      | Types.ExceptionType exn, env2, rules2 -> (Types.ExceptionType exn, env2, rules1 @ rules2 @ [("(" ^ Types.string_of_error exn ^ ") || ExnPropag. (err 2)", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Types.string_of_error exn)])
      | t2, env2, rules2 -> (Types.Pair (t1, t2), env2, rules1 @ rules2 @ [("T-Pair", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": (" ^ Types.string_of_tipo t1 ^ " * " ^ Types.string_of_tipo t2 ^ ")")]))
  )


  (* fst e *)
  | Terms.Fst e -> (match typeinfer e env with
    | Types.ExceptionType exn, env', rules -> (Types.ExceptionType exn, env', rules @ [("(" ^ Types.string_of_error exn ^ ") || ExnPropag. (err 3)", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Types.string_of_error exn)])
    | Types.Pair (t1, t2), env', rules -> (t1, env', rules @ [("T-Fst", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Types.string_of_tipo t1)])
    | _ -> (ExceptionType TypeError, env, [("TypeError (err 4)", "argumento de `fst e` deve ser um par ordenado, mas foi " ^ Terms.ast_of_term e)])
  )


  (* snd e *)
  | Terms.Snd e -> (match typeinfer e env with
    | Types.ExceptionType exn, env', rules -> (Types.ExceptionType exn, env', rules @ [("(" ^ Types.string_of_error exn ^ ") || ExnPropag. (err 5)", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Types.string_of_error exn)])
    | Types.Pair (t1, t2), env', rules -> (t2, env', rules @ [("T-Snd", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Types.string_of_tipo t2)])
    | _ -> (ExceptionType TypeError, env, [("TypeError (err 6)", "argumento de `snd e` deve ser um par ordenado, mas foi " ^ Terms.ast_of_term e)])
  )

  (* if e1 then e2 else e3 *)
  | Terms.Conditional (e1, e2, e3) -> (
    let t1, env', rules = typeinfer e1 env in (match t1 with
      | Types.ExceptionType exn -> (Types.ExceptionType exn, env', rules @ [("(" ^ Types.string_of_error exn ^ ") || ExnPropag. (err 7)", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Types.string_of_error exn)])
      | Types.Bool -> (
          let t2, env'', rules'   = typeinfer e2 env' in (match t2 with
            | Types.ExceptionType exn -> (Types.ExceptionType exn, env'', rules' @ [("(" ^ Types.string_of_error exn ^ ") || ExnPropag. (err 8)", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Types.string_of_error exn)])
            | t2 -> (
                let t3, env''', rules'' = typeinfer e3 env'' in (
                  if t3 = t2 then
                (t3, env''', rules @ rules' @ rules'' @ [("T-If", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Types.string_of_tipo t3)])
                  else
                (Types.ExceptionType TypeError, env'', rules' @ [("TypeError (err 9)", "tipos de `e2` e `e3` devem ser iguais, mas sao " ^ Types.string_of_tipo t2 ^ " e " ^ Types.string_of_tipo t3)])
                )
              )
            ))

      | _ -> (Types.ExceptionType TypeError, env', rules @ [("TypeError (err 10)", "argumento de `if e1 then e2 else e3` deve ser um booleano, mas foi " ^ Terms.ast_of_term e1)])
      )
    )


  | Raise e -> (match typeinfer e env with
    | _ -> (Types.ExceptionType (Types.NotImplemented " wildcard"), env, [("RaiseWildExn", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": exn")]))
  

  | _ -> failwith "typeinfer"
);;