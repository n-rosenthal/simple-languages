(*
  Avaliador small-step para L1
*)

exception RuntimeError of string;;

let typeof (e: Terms.term) (env: Middleware.env) = (let t, _, _ = Typeinfer.typeinfer e env in t);;

let is_exn (e: Terms.term) (env: Middleware.env) = (match typeof e env with Types.ExceptionType _ -> true | _ -> false);;

let is_pair (e: Terms.term) (env: Middleware.env) = (match typeof e env with Types.Pair _ -> true | _ -> false);;

let is_true (e: Terms.term) = (match e with Terms.Boolean true -> true | _ -> false)
and is_false (e: Terms.term) = (match e with Terms.Boolean false -> true | _ -> false)
and is_bool (e: Terms.term) (env: Middleware.env) = (match typeof e env with Types.Bool -> true | _ -> false)
and eq_type (e1: Terms.term) (e2: Terms.term) (env: Middleware.env) = (
  let t1, _, _ = Typeinfer.typeinfer e1 env in
  let t2, _, _ = Typeinfer.typeinfer e2 env in
  if t1 = t2 then true else false
);

(* ambiente de valores para L1 *)
type binding  = (Terms.term * Terms.value * string);; (* e, v, x*)
type env      = binding list
and string_of_env = string -> string;;

let rec string_of_env (env: env) : string = (
  let rec aux (env: env) : string = (
    match env with
    | [] -> "Γ"
    | (e, v, x)::tl -> "(" ^ x ^ ", " ^ Terms.string_of_term e ^ ", " ^ Terms.string_of_value v ^ ") :: " ^ aux tl
  )
  in (
    "[" ^ aux env ^ "]"
  )
);;

let rec step (e: Terms.term) (env: env) (types: Middleware.env)
           : (Terms.term * env * (string * string) list) =
  match e with
    | Terms.None ->
        (Terms.None, env,
          [("E-None", string_of_env env ^ " ⊢ () : unit")])

    | Terms.Integer n ->
        (Terms.Integer n, env,
          [("E-Int", string_of_env env ^ " ⊢ " ^ string_of_int n ^ " : int")])

    | Terms.Boolean b ->
        (Terms.Boolean b, env,
          [("E-Bool", string_of_env env ^ " ⊢ " ^ string_of_bool b ^ " : bool")])

    (* e1 -> e1' => (e1, e2) -> (e1', e2) *)
    | Terms.OrderedPair (e1, e2) when not (Terms.is_value e1) ->
        let (e1', env1, rules1) = step e1 env types in
        let step_term = Terms.OrderedPair (e1', e2) in
        let rule =
          ( "E-Pair1"
          , string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e1 ^ " -> " ^ Terms.ast_of_term e1'
            ^ " ==> " ^ Terms.ast_of_term (Terms.OrderedPair (e1, e2))
            ^ " -> " ^ Terms.ast_of_term step_term )
        in
        (step_term, env1, rules1 @ [rule])

    (* (exn, e2) -> exn *)
    | Terms.OrderedPair (e1, e2) when is_exn e1 types ->
        (e1, env,
          [("E-Prop.Exn. Pair 1", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Terms.string_of_term e1)])
    
    (* e2 -> e2' => (v1, e2) -> (v1, e2') *)
    | Terms.OrderedPair (e1, e2) when Terms.is_value e1 && not (Terms.is_value e2) ->
        let (e2', env2, rules2) = step e2 env types in
        let step_term = Terms.OrderedPair (e1, e2') in
        let rule =
          ( "E-Pair2"
          , string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e2 ^ " -> " ^ Terms.ast_of_term e2'
            ^ " ==> " ^ Terms.ast_of_term (Terms.OrderedPair (e1, e2))
            ^ " -> " ^ Terms.ast_of_term step_term )
        in
        (step_term, env2, rules2 @ [rule])

    (* (v1, exn) -> exn *)
    | Terms.OrderedPair (e1, e2) when Terms.is_value e1 && is_exn e2 types ->
        (e2, env,
          [("E-Prop.Exn. Pair 2", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Terms.string_of_term e2)])
    
    (* v1, v2 -> (v1, v2) *)
    | Terms.OrderedPair (e1, e2) when Terms.is_value e1 && Terms.is_value e2 ->
      let t, _, _ = Typeinfer.typeinfer e types in 
      (e, env,
        [("Value", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ " : " ^ Types.string_of_tipo t)])
    
    (* fst exn -> exn *)
    | Terms.Fst e when is_exn e types ->
        (e, env,
          [("E-Prop.Exn. Fst", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Terms.string_of_term e)])
    
    (* e !: (t, t) => fst e -> raise exn *)
    | Terms.Fst e when not (is_pair e types) ->
        (Terms.Raise (Terms.RuntimeError "BadArg Fst"),
        env,
        [("BadArg Fst", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Terms.string_of_term e)])
    
    (* fst (v1, v2) -> v1 *)
    | Terms.Fst e when is_pair e types -> (match e with
      | Terms.OrderedPair (e1, e2) ->
        (e1, env,
          [("E-Fst", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Terms.string_of_term e1)])
      | _ -> failwith "typeinfer")

    (* snd exn -> exn *)
    | Terms.Snd e when is_exn e types ->
        (e, env,
          [("E-Prop.Exn. Snd", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Terms.string_of_term e)])
    
    (* e !: (t, t) => snd e -> raise exn *)
    | Terms.Snd e when not (is_pair e types) ->
        (Terms.Raise (Terms.RuntimeError "BadArg Snd"),
        env,
        [("BadArg Snd", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Terms.string_of_term e)])
    
    (* snd (v1, v2) -> v2 *)
    | Terms.Snd e when is_pair e types -> (match e with
      | Terms.OrderedPair (e1, e2) ->
        (e2, env,
          [("E-Snd", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Terms.string_of_term e2)])
      | _ -> failwith "typeinfer")
    
    (* Conditional *)
    (* if exn? then exn? else exn ->? exn *)
    (** XXX há uma ordem de preferência na propagação de exceções aqui. *) 
    | Terms.Conditional (e1, e2, e3) when is_exn e1 types || is_exn e2 types || is_exn e3 types -> (
      if is_exn e1 types then
        (e1, env,
          [("E-Prop.Exn. If 1", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Terms.string_of_term e1)])
      else if is_exn e2 types then
        (e2, env,
          [("E-Prop.Exn. If 2", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Terms.string_of_term e2)])
      else
        (e3, env,
          [("E-Prop.Exn. If 3", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Terms.string_of_term e3)])
    )
    
    (* e1 -> e1' => if e1 then e2 else e3 -> if e1' then e2 else e3 *)
    | Terms.Conditional (e1, e2, e3) when not (Terms.is_value e1) ->
        let (e1', env1, rules1) = step e1 env types in
        (Terms.Conditional (e1', e2, e3), env1, rules1 @
          [("E-If1", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e1 ^ " -> " ^ Terms.ast_of_term e1' ^ " : " ^ Terms.string_of_term e1')])
    
    (* e2 -> e2' => if v1 then e2' else e3 -> if v1 then e2' else e3 *)
    | Terms.Conditional (e1, e2, e3) when (Terms.is_value e1) && (not (Terms.is_value e2)) ->
        let (e2', env2, rules2) = step e2 env types in
        (Terms.Conditional (e1, e2', e3), env2, rules2 @
          [("E-If2", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e2 ^ " -> " ^ Terms.ast_of_term e2' ^ " : " ^ Terms.string_of_term e2')])
    
    (* if true then e2 else e3 -> e2 *)
    (* v1, v2 && v1 = true => v2 *)
    | Terms.Conditional (e1, e2, e3) when (Terms.is_value e1) && (Terms.is_value e2) && (is_true e1) ->
        (e2, env,
          [("E-If True", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Terms.string_of_term e2)])
    
    (* v1 = false, v2 && e3 -> e3' => if false then v2 else e3 -> if false then v2 else e3' *)
    (* if false then v2 else e3 -> if false then v2 else e3' *)
    | Terms.Conditional (e1, e2, e3) when (Terms.is_value e1) && (Terms.is_value e3) && (is_false e1) ->
        let (e3', env3, rules3) = step e3 env types in
        (Terms.Conditional (e1, e2, e3'), env3, rules3 @
          [("E-If3", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Terms.string_of_term e3')])
    
    (* v1 = false, v2, v3 => if false then v2 else v3 -> v3 *)
    | Terms.Conditional (e1, e2, e3) when (Terms.is_value e1) && (Terms.is_value e2) && (Terms.is_value e3) && (is_false e1) ->
        (e3, env,
          [("E-If False", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Terms.string_of_term e3)])
    
    (* e1 !: Bool => if e1 then e2 else e3 -> exn *)
    | Terms.Conditional (e1, e2, e3) when not (is_bool e1 types) -> (
        let (e1', env1, rules1) = step e1 env types in (match e1' with
          | Terms.Boolean b ->
              (Terms.Conditional (e1', e2, e3), env1, rules1 @
                [("E-If Bool", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e1 ^ ": " ^ Terms.string_of_term e1')])
          | _ ->
              (Terms.Raise (Terms.TypeError ("condição e1 de um If (e1, e2, e3) deve ser do tipo bool, mas foi " ^ Terms.ast_of_term e1)), env1, rules1 @
                [("E-If Bool", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e1 ^ ": " ^ Terms.string_of_term e1)]))
      )

    (* e2: t, e3: t', t <> t' => if e1 then e2 else e3 -> exn *)
    | Terms.Conditional (e1, e2, e3) when not (eq_type e2 e3 types) ->
        (Terms.Raise (Terms.TypeError ("condição e2 e e3 de um If (e1, e2, e3) devem ser do mesmo tipo, mas foram " ^ Terms.string_of_term e2 ^ " e " ^ Terms.string_of_term e3)), env,
          [("E-If Type", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Terms.string_of_term e2 ^ " e " ^ Terms.string_of_term e3)])

    
    (* Definição de variável, let x = e1 in e2 *)
    (* e1 = exn => let x = e1 in e2 -> exn *)
    | Terms.VarDefinition (x, e1, e2) when (is_exn e1 types) ->
      (e1, env,
        [("E-Let Exn 1", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Terms.string_of_term e1)])
    
    (* e1 -> e1' => let x = e1 in e2 -> let x = e1' in e2 *)
    | Terms.VarDefinition (x, e1, e2) when not (Terms.is_value e1) ->
        let (e1', env1, rules1) = step e1 env types in
        (Terms.VarDefinition (x, e1', e2), env1, rules1 @
          [("E-Let 1", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Terms.string_of_term e1')])
    
    (* e2 = exn => let x = e1 in e2 -> exn *)
    | Terms.VarDefinition (x, e1, e2) when (is_exn e2 types) ->
        (e2, env,
          [("E-Let Exn 2", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Terms.string_of_term e2)])
    
    (* let x = v1 in e2 *)
    | Terms.VarDefinition (x, v1, e2) when (Terms.is_value v1) -> (
      let env' = (v1, Terms.value_of_term v1, x) :: env in
      let rule = ("E-Let 2", string_of_env env ^ " ⊢ {" ^ x ^ ": " ^ Terms.string_of_term v1 ^ "} ⊢ " ^ Terms.ast_of_term e ^ ": " ^ Terms.string_of_term e2) in
      (e2, env', [rule])
    )

    (* Identifier x *)
    | Terms.Identifier x ->
        let (v, _, _) = List.find (fun (v, _, x') -> x = x') env in
        (v, env,
          [("E-Var", string_of_env env ^ " ⊢ " ^ x ^ " : " ^ Terms.string_of_term v)])
      
    | _ ->
        failwith "não implementado"
;;



(* driver: repeatedly apply step until a value, collecting rules *)
let rec eval (e: Terms.term) (env: env) (types: Middleware.env)
  : Terms.value * (string * string) list = 
  if Terms.is_value e then
    (* convert term to value *)
    (match e with
      | Terms.None -> (Unit, [("V-None", string_of_env env ^ " ⊢ () : unit")])
      | Terms.Integer n -> (VInt n, [("V-Int", string_of_env env ^ " ⊢ " ^ string_of_int n ^ " : int")])
      | Terms.Boolean b -> (VBool b, [("V-Bool", string_of_env env ^ " ⊢ " ^ string_of_bool b ^ " : bool")])
      | Terms.OrderedPair (e1, e2) ->
          (VPair (Terms.value_of_term e1, Terms.value_of_term e2), [("V-Pair", string_of_env env ^ " ⊢ " ^ Terms.ast_of_term e ^ " : " ^ Types.string_of_tipo (typeof e types))])
      | _ -> raise (RuntimeError ("the value `" ^ Terms.string_of_term e ^ "` is not a value")))
  else
    let (e', env', rules1) = step e env types in
    let (v, rules2) = eval e' env' types in
    (v, rules1 @ rules2)
;;
