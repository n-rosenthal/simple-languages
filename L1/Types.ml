(*
  Sistema de tipos e inferência estática de tipos para L1
*)

(*  tipos de L1 *)
type tipo =
  | NoneType
  | Bool
  | Int
  | Pair of tipo * tipo
;;

(* repr. string de um tipo *)
let rec string_of_tipo (t: tipo) : string = match t with
  | NoneType -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | Pair (t1, t2) -> "(" ^ string_of_tipo t1 ^ " * " ^ string_of_tipo t2 ^ ")"
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
    let rec typeinfer (e: Terms.term) (env: env) : (tipo * env * Repr.schema list, exn) result =
  match e with
  (* Unit (), None *)
  | Terms.None -> Ok (NoneType, env, [("T-Unit", [])])

  (* Integer n *)
  | Terms.Integer n -> Ok (Int, env, [("T-Int", [Terms.string_of_term e; string_of_env env])])

  (* Boolean b *)
  | Terms.Boolean b -> Ok (Bool, env, [("T-Bool", [Terms.string_of_term e; string_of_env env])])

  (* (e1, e2) *)
  | Terms.OrderedPair (e1, e2) -> (match typeinfer e1 env with
    | Ok (t1, env1, rules1) -> (match typeinfer e2 env1 with
      | Ok (t2, env2, rules2) -> Ok (Pair (t1, t2), env2, rules1 @ 
                                                          rules2 @ 
                                                          [("T-OrderedPair", [
                                                            Terms.string_of_term e1; string_of_tipo t1;
                                                            Terms.string_of_term e2; string_of_tipo t2;
                                                            Terms.string_of_term e; 
                                                            string_of_env env
                                                          ])])
      | Error exn -> Error exn)
    | Error exn -> Error exn)

  (* fst e *)
  | Terms.Fst e -> (match typeinfer e env with
    | Ok (Pair (t1, t2), env1, rules1) -> Ok (t1, env1, rules1 @ 
                                                        [("T-Fst", [
                                                          Terms.string_of_term e; 
                                                          string_of_tipo t1;
                                                          string_of_tipo t2;
                                                          string_of_env env
                                                        ])])
    | Ok (t, env1, rules1) -> Error (TypeError ("argument of `fst e` must be a pair", Some e))
    | Error exn -> Error exn)

  (* snd e *)
  | Terms.Snd e -> (match typeinfer e env with
    | Ok (Pair (t1, t2), env1, rules2) -> Ok (t2, env1, rules2 @ [("T-Snd", [
                                                                  Terms.string_of_term e; 
                                                                  string_of_tipo t1;
                                                                  string_of_tipo t2;
                                                                  string_of_env env
                                                                ])])
    | Ok (t, env1, rules1) -> Error (TypeError ("argument of `snd e` must be a pair", Some e))
    | Error exn -> Error exn)

  (* if e1 then e2 else e3 *)
  | Terms.Conditional (e1, e2, e3) -> (match typeinfer e1 env with
    | Ok (Bool, env', rules1) -> (match typeinfer e2 env' with
      | Ok (t2, env'', rules2) -> (match typeinfer e3 env'' with
        | Ok (t3, env''', rules3) -> (match t2, t3 with
          | t2, t3 when t2 = t3 -> Ok (t2, env''', rules1 @ rules2 @ rules3 @ [("T-If", [
            Terms.string_of_term e1;
            Terms.string_of_term e2;
            Terms.string_of_term e3;
            string_of_tipo t2;
            string_of_env env
          ])])
          | t2, t3 -> Error (TypeError ("branches of `if e1 then e2 else e3` must have same type", Some e))
        )
        | Error exn -> Error exn)
      | Error exn -> Error exn)
    | Ok (t, env', rules') -> Error (TypeError ("condition of `if e1 then e2 else e3` must be a boolean", Some e))
    | Error exn -> Error exn)

  | Terms.Identifier x -> (match lookup x env with
    | Some (e', t) -> Ok (t, env, [("T-Var", [
                                              x;
                                              string_of_tipo t;
                                              string_of_env env
                                            ])])
    | None -> Error (TypeError ("Unbound identifier `" ^ x ^ "`", Some e)))

  (* let x = e1 in e2 *)
  | Terms.VarDefinition (nb, e2) -> 
    (*  verifica se o binding `nb` está no ambiente atual; se sim, verifica igualdade e atualiza se necessário; senão, adiciona *)
    (match lookup (snd nb) env with
    | Some (e1, t1) -> (match typeinfer e1 env with
      | Ok (t2, env1, rules1) -> (match t1, t2 with
        | t1, t2 when t1 = t2 -> Ok (t1, env1, rules1 @ [("T-Let", [
          Terms.string_of_term e1;
          string_of_tipo t1;
          snd nb;
          Terms.string_of_term e2;
          string_of_tipo t2;
          string_of_env env
        ])])
        | t1, t2 -> Error (TypeError ("bindings of `let x = e1 in e2` must have same type", Some e))
      )
      | Error exn -> Error exn)
    | None -> (match typeinfer (fst nb) env with
      | Ok (t1, env1, rules1) -> (match typeinfer e2 ((nb, t1) :: env1) with
        | Ok (t2, env', rules2) -> Ok (t2, env', rules1 @ rules2 @ [("T-Let", [
          Terms.string_of_term (fst nb);
          string_of_tipo t1;
          snd nb;
          Terms.string_of_term e2;
          string_of_tipo t2;
          string_of_env env
        ])])
        | Error exn -> Error exn)
      | Error exn -> Error exn))

  | _ -> Error (TypeError ("typeinfer failed", Some e))
;;
