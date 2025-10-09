(**
  Avaliador de expressões da linguagem L2
  Define `context, contextos para avaliação de expressões.
  Regras de avaliação são usadas de forma análoga às regras de inferência de tipo. As estruturas `rule` e `rules` (string * string e string * string list, respec.) são análogas àquelas definidas em `TypeInference.ml`.

  De forma análoga ao inferidor de tipos, estendemos a sintaxe de expressões para incluir "RuntimeError", que representa **qualquer** tipo de erro de avaliação, exceção, comportamento indefinido ou outros comportamentos ainda não planejados, ou imprevistos. Estender a sintaxe de valores também se tornou necessário, incluindo aqui o valor mais genérico "Error".


  version 1.0
*)

open Types
open Terms


let is_exn (e: term) : bool = match e with
  | RuntimeError _ -> true
  | _ -> false
;;

let is_true (e: term) : bool = match e with
  | Boolean true -> true
  | _ -> false
and is_false (e: term) : bool = match e with
  | Boolean false -> true
  | _ -> false
;;

type rules = rule list
and rule = string * string
;;



(* repr. de uma regra concreta de avaliação de expressão *)
let string_of_rule (x: string) (e: string) : string = (
  "[" ^ x ^ "] " ^ e
);;

(* repr. enumerada de uma lista de regras de avaliação de expressão *)
let string_of_rules (rules: rules) : string list =
  let repr = ref [] in
  List.iteri (fun i (x, e) ->
    repr := ("(" ^ string_of_int i ^ ") " ^ string_of_rule x e) :: !repr
  ) rules;
  List.rev !repr
;;

let print_rules (rules: rules) : unit = (
  List.iter (fun (x, e) -> print_endline (string_of_rule x e)) rules
)
;;

type context = (string * term) list
and lookup = string -> context -> term option
and insert = string -> term -> context -> context
and update = string -> term -> context -> context
;;

(* retorna um termo dado um identificador `x` e um contexto `ctx`, se `x` estiver no contexto `ctx` *)
let rec lookup (x: string) (ctx: context) : term option = (
  match ctx with
  | [] -> None
  | (y, e)::tl -> if x = y then Some e else lookup x tl
);;

(* atualiza um par (identificador * termo) no contexto `ctx` *)
let update (x: string) (e: term) (ctx: context) : context = (
  List.map (fun (y, e') -> if x = y then (x, e) else (y, e')) ctx
);;

(* insere um par (identificador * termo) no contexto `ctx` *)
let insert (x: string) (e: term) (ctx: context) : context = (
  (x, e) :: ctx
);;

(* repr. string de um contexto *)
let string_of_context (ctx: context) : string = (
  let rec aux (ctx: context) : string = (
    match ctx with
    | [] -> "σ"
    | (x, e)::tl -> "(" ^ x ^ ", " ^ string_of_term e ^ ") :: " ^ aux tl
  )
  in (
    "[" ^ aux ctx ^ "]"
  )
);;


(**
  Avaliador de expressões da linguagem L2
  
*)

(* faz um passo de avaliação de expressão *)
let rec step (e: term) (ctx: context) : (term * context * rules) = (match e with
  (** valores *)
  | Nothing -> (e, ctx, [("V-Unit", string_of_context ctx ^ " ⊢ " ^ string_of_term e ^ " : unit")])
  | Integer _ -> (e, ctx, [("V-Integer", string_of_context ctx ^ " ⊢ " ^ string_of_term e ^ " : int")])
  | Boolean _ -> (e, ctx, [("V-Boolean", string_of_context ctx ^ " ⊢ " ^ string_of_term e ^ " : bool")])

  (** if e1 then e2 else e3 *)
  (* is_exn(e1) => if e1 then e2 else e3 -> RuntimeError *)
  | Conditional (e1, e2, e3) when is_exn e1 -> (
    (e1, ctx, [("E-If Prop. Exn 1", ast_of_term e1 ^ " => " ^ ast_of_term (Conditional (e1, e2, e3)) ^ " -> " ^ ast_of_term e1)])
  )

  (* !is_exn(e1) && !is_value(e1) *)
  (* e1 -> e1' => if e1 then e2 else e3 -> if e1' then e2 else e3 *)
  | Conditional (e1, e2, e3) when not (is_value e1) && not (is_exn e1) -> (
    let (e1', ctx1, rules1) = step e1 ctx in
    (Conditional (e1', e2, e3), ctx1, rules1 @ [("E-If1", string_of_context ctx ^ " ⊢ " ^ ast_of_term e1 ^ " -> " ^ ast_of_term e1' ^ " : " ^ string_of_term e1')])
  )

  (* v1, is_exn(e2) => if e1 then e2 else e3 -> RuntimeError *)
  | Conditional (e1, e2, e3) when is_exn e2 -> (
    (e2, ctx, [("E-If Prop. Exn 2", ast_of_term e2 ^ " => " ^ ast_of_term (Conditional (e1, e2, e3)) ^ " -> " ^ ast_of_term e2)])
  )

  (* v1, is_exn(e3) => if e1 then e2 else e3 -> RuntimeError *)
  | Conditional (e1, e2, e3) when is_exn e3 -> (
    (e3, ctx, [("E-If Prop. Exn 3", ast_of_term e3 ^ " => " ^ ast_of_term (Conditional (e1, e2, e3)) ^ " -> " ^ ast_of_term e3)])
  )

  (* v1 : T, T <> Bool, !is_exn(e2), !is_exn(e3) => if e1 then e2 else e3 -> TypeError *)
  | Conditional (e1, e2, e3) when not (is_exn e2) && not (is_exn e3) && TypeInference.typeof e1 <> Bool -> (
    (RuntimeError "type mismatch", ctx, [("E-If TypeMismatch", "type mismatch")])
  )

  (* v1 : Bool, e2 : T, e3 : T', T <> T => if e1 then e2 else e3 -> TypeError *)
  | Conditional (e1, e2, e3) when (TypeInference.typeof e2 <> TypeInference.typeof e3) -> (
    (RuntimeError "type mismatch", ctx, [("E-If TypeMismatch", "type mismatch")])
  )

  (* v1 : Bool = True, e2 : T, e3 : T *)
  | Conditional (v1, e2, e3) when (is_true v1 && (TypeInference.typeof e2 = TypeInference.typeof e3)) -> (
    (e2, ctx, [("E-If2", string_of_context ctx ^ " ⊢ " ^ ast_of_term e2 ^ " : " ^ string_of_term e2)])
  )

  (* v1 : Bool = False, e2 : T, e3 : T *)
  | Conditional (v1, e2, e3) when (is_false v1 && (TypeInference.typeof e2 = TypeInference.typeof e3)) -> (
    (e3, ctx, [("E-If3", string_of_context ctx ^ " ⊢ " ^ ast_of_term e3 ^ " : " ^ string_of_term e3)])
  )




  (* Identifier x *)
  | Identifier x -> (match lookup x ctx with
    | Some e -> (e, ctx, [("V-Var", string_of_context ctx ^ " ⊢ " ^ string_of_term e ^ " : " ^ string_of_term e)])
    | None -> (RuntimeError "variável desconhecida", ctx, [("E-Error UnboundIdentifier", "variável desconhecida")])
  )

  (* let x : T = e1 in e2 *)
  (* is_exn(e1) => let x : T = RuntimeError in e2 -> RuntimeError *)
  | VarDefinition (x, e1, t, e2) when is_exn e1 -> (
    (e1, ctx, [("E-Let Prop. Exn 1", ast_of_term e1 ^ " => " ^ ast_of_term (VarDefinition (x, e1, t, e2)) ^ " -> " ^ ast_of_term e1)])
  )

  (* is_exn(e2) => let x : T = e1 in e2 -> RuntimeError *)
  | VarDefinition (x, e1, t, e2) when is_exn e2 -> (
    (e2, ctx, [("E-Let Prop. Exn 2", ast_of_term e2 ^ " => " ^ ast_of_term (VarDefinition (x, e1, t, e2)) ^ " -> " ^ ast_of_term e2)])
  )

  (* e1 -> e1' => let x : T = e1 in e2 -> let x : T = e1' in e2  *)
  | VarDefinition (x, e1, t, e2) when not (is_value e1) -> (
    print_endline "let 1";
    let (e1', ctx', rules') = step e1 ctx in
    let rule = ("E-Let 1", ast_of_term e1 ^ " -> " ^  ast_of_term e1' ^ " => " ^ ast_of_term (VarDefinition (x, e1, t, e2)) ^ " -> " ^ ast_of_term (VarDefinition (x, e1', t, e2))) in
      (VarDefinition (x, e1', t, e2), ctx', rules' @ [rule])
    )

  (* v1 : T', T <> T' *)
  | VarDefinition (x, e1, t, e2) when (is_value e1 && TypeInference.typeof e1 <> t) -> (
    (RuntimeError "type mismatch", ctx, [("E-Err Let TypeMismatch", "type mismatch")])
  )

  (* v1, e2, e1 : T', T = T'  let x : T = v1 in e2 => {x/v} e2 *)
  (* substituição beta *)
  | VarDefinition (x, e1, t, e2) when (is_value e1 && TypeInference.typeof e1 = t) -> (
    let ctx' = insert x e1 ctx in  (* CORREÇÃO: usar insert em vez de update *)
    let rule = ("E-Let 2", ast_of_term e1 ^ " => " ^ ast_of_term (VarDefinition (x, e1, t, e2)) ^ " -> " ^ ast_of_term e2) in
    (e2, ctx', [rule])
  )

  | _ -> (RuntimeError "não implementado", ctx, [("E-Error NotImplemented", "não implementado: " ^ ast_of_term e ^ "\n\t" ^ string_of_term e)])
);;

(* faz tantos passos quanto forem necessários até avaliar `e` em um valor `v`, ou em um erro. *)
let rec eval (e: term) (ctx: context) : (value * context * rules) = (
  match (step e ctx) with
    | (RuntimeError s, ctx, rules) -> (Error s, ctx, rules)
    | (v, ctx, rules) when (is_value v) -> (value_of_term v, ctx, rules)
    | (e, ctx, rules) -> (
        let (e', ctx', rules') = eval e ctx in
        (e', ctx', rules @ rules')
    )
);;

