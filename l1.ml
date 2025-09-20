(*
  Implementação da linguagem L1 (`arith` ou cálculo lambda tipado, extendido sobre operações aritméticas). L1 é uma linguagem de TERMOS (i.e. expressões, neste caso (!)).

  Implementação:
    - inferidor de tipos para L1
    - avaliador de termos para L1
    - interpretador para L1
*)
open Re


(*
  =============================================================
  Erros, exceções, IO
  =============================================================
*)
exception ProgrammingError of string;;
exception TypeError of string;;
exception RuntimeError of string;;


(* 
  ==============================================================
  Sistema de tipos para L1
  ==============================================================
*)
type tipo =
  | Bool
  | Int
  | OrderedPair of tipo * tipo
  | Arrow       of tipo * tipo
  | RecursiveFn of tipo  (* restricted: t → t *)

(* --- string repr. tipos --------------------------------------- *)
(* repr. string de tipos *)
let rec string_of_tipo (t: tipo) : string = match t with
  | Bool -> "bool"
  | Int -> "int"
  | OrderedPair (t1, t2) -> string_of_tipo t1 ^ " * " ^ string_of_tipo t2
  | Arrow (t1, t2) -> "(" ^ string_of_tipo t1 ^ " → " ^ string_of_tipo t2 ^ ")"
  | RecursiveFn t1 -> "(" ^ string_of_tipo t1 ^ " → " ^ string_of_tipo t1 ^ ")"
;;


(* --- rule schema para o sist. tipos --------------------------- *)
(* type system rules as schematic templates *)
let type_scheme_rules : (string * (string * string list)) list = [
  ("[T-Int]",   ("Γ ⊢ e : int",  ["e"]));
  ("[T-Bool]",  ("Γ ⊢ e : bool", ["e"]));
  ("[T-Pair]",  ("Γ ⊢ e1 : t1    \t Γ ⊢ e2 : t2   \t Γ ⊢ (e1,e2) : t1 * t2", 
                ["e1"; "t1"; "e2"; "t2"; "(e1,e2)"]));
  ("[T-Arrow]", ("Γ ⊢ e1 : t1 → t2   \t Γ ⊢ e2 : t1   \t Γ ⊢ e1 e2 : t2", 
                ["e1"; "t1"; "t2"; "e2"; "e1 e2"]));
  ("[T-RecFn]", ("Γ, f : t → t ⊢ e : t   \t Γ ⊢ rec f.e : t → t",
                ["f"; "t"; "e"; "rec f.e"]));
  
  ("[T-If]",    ("Γ ⊢ e1 : bool   \t Γ ⊢ e2 : t   \t Γ ⊢ e3 : t   \t Γ ⊢ if e1 then e2 else e3 : t",
                ["e1"; "e2"; "e3"; "t"; "if e1 then e2 else e3"]));

  (* fst e, snd e *)
  ("[T-Fst]",   ("Γ ⊢ e : t1 * t2   \t Γ ⊢ fst e : t1", ["e"; "t1"; "t2";]));
  ("[T-Snd]",   ("Γ ⊢ e : t1 * t2   \t Γ ⊢ snd e : t2", ["e"; "t1"; "t2";]));

  (*  var *)
  ("[T-Var]",   ("Γ(x) = t → ⊢ x : t", ["x"; "t"]));

  (*  let*)
  ("[T-Let]",   ("Γ ⊢ e1 : T, Γ[x |→ T'] ⊢ e2 : T' --> Γ ⊢ let x : T = e1 in e2 : T' ",
                ["e1"; "t1"; "x"; "e2"; "t2"; "let x = e1 in e2"]));
  
  
];;

(* replace all occurrences of x by x' in s *)
let replace (s : string) (x : string) (x' : string) : string =
  Re.replace_string (Re.compile (Re.str x)) ~by:x' s
;;

let replace_list (s : string) (xs : string list) (xs' : string list) : string =
  List.fold_left2 (fun acc x x' -> replace acc x x') s xs xs'
;;

let get_typerule (rule_name: string) (concretes: string list) : (string, exn) result =
  let (rule, metavars) = List.assoc rule_name type_scheme_rules in
  let rule' = replace_list rule metavars concretes in
  Ok rule'
;;

(* ============================================================= 
  Sintaxe de EXPRESSÕES (termos) sobre L1
  ============================================================== *)
  type term =
  | Integer         of int                          (* valor: inteiro           *)
  | Boolean         of bool                         (* valor: booleano          *)
  | Pair            of term * term                  (* valor: par v,u           *)
  | Snd             of term                         (* snd e                    *)
  | Fst             of term                         (* fst e                    *)
  | Conditional     of term * term * term           (* if e1 then e2 else e3    *)
  | Identifier      of string                       (* x, identificador         *)
  | VarDefinition   of string * tipo * term * term  (* let x : t = e1 in e2     *)

(*
  | BinaryOperation of binop  * term * term         (* e1 op e2                 *)
  | Application     of term * term                  (* e1 e2                    *)
  | Function        of string * tipo * term         (* fun x : t -> e           *)
  | RecFunction     of string * tipo * term * term  (* let rec f : t = e1 in e2 *)
*)
;;

type value =
  | VInt      of int
  | VBool     of bool
  | VPair     of value * value
  | VFn       of string * term
  | VRecFn    of string * term
;;

(*  repr. string de um termo em L1 *)
let rec string_of_term (e: term) : string = (match e with
    | Integer i -> string_of_int i
    | Boolean b -> string_of_bool b
    | Pair (e1, e2) -> "(" ^ string_of_term e1 ^ ", " ^ string_of_term e2 ^ ")"
    | Conditional (e1, e2, e3) -> "if " ^ string_of_term e1 ^ " then " ^ string_of_term e2 ^ " else " ^ string_of_term e3
    | Fst e -> "fst " ^ string_of_term e
    | Snd e -> "snd " ^ string_of_term e
    | Identifier x -> x
    | VarDefinition (x, t, e1, e2) -> "let " ^ x ^ " : " ^ string_of_tipo t ^ " = " ^ string_of_term e1 ^ " in " ^ string_of_term e2
  );;

let rec string_of_value (v: value) : string = (match v with
    | VInt i -> string_of_int i
    | VBool b -> string_of_bool b
    | VPair (v1, v2) -> "(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ")"
    | VFn (x, e) -> "fun " ^ x ^ " -> " ^ string_of_term e
    | VRecFn (f, e) -> "rec " ^ f ^ " -> " ^ string_of_term e
  );;


(* convert term -> value (only valid if term is a value) *)
  let rec value_of_term t =
    match t with
    | Integer n -> VInt n
    | Boolean b -> VBool b
    | Pair (a, b) -> VPair (value_of_term a, value_of_term b)
    | _ -> failwith ("value_of_term: not a value")
  
  (* convert value -> term *)
  let rec term_of_value v =
    match v with
    | VInt n -> Integer n
    | VBool b -> Boolean b
    | VPair (v1, v2) -> Pair (term_of_value v1, term_of_value v2)
    | VFn (x, e) -> failwith ("term_of_value: not a term")
    | VRecFn (f, e) -> failwith ("term_of_value: not a term")
    | _ -> failwith ("term_of_value: not a term")
  ;;
  
  (* is this term already a value? *)
  let rec is_value_term t =
    match t with
    | Integer _ | Boolean _ -> true
    | Pair (a, b) -> is_value_term a && is_value_term b
    | _ -> false
  ;;
  

(* --- ambiente de tipos ---------------------------------------- *)
type env = (string * term * tipo) list;;

(* busca por um identificador `x` no ambiente de tipos Γ e retorna o seu tipo `t`, se existir *)
let rec lookup (x: string) (envtypes: env) : (term * tipo)  option = (
  match envtypes with
  | [] -> None
  | (y, e, t)::tl -> if x = y then Some (e, t) else lookup x tl 
);;



(* ============================================================= 
  Inferência Estática de Tipos para L1
  ============================================================== *)

(* infere o tipo de um termo `e` sobre um ambiente de tipos `Γ`
  @returns (tipo de `e`, lista de regras usadas) 
  @raises TypeError se `e` for mal-formado *)
let rec typeinfer (e: term) (envtypes: env) : (tipo * (string * string list) list) = (
  match e with
  | Integer _ -> (Int, [("T-Int", [string_of_term e])])
  | Boolean _ -> (Bool, [("T-Bool", [string_of_term e])])


  | Pair (e1, e2) -> (
    let (t1, rules1) = typeinfer e1 envtypes in
    let (t2, rules2) = typeinfer e2 envtypes in
    let pair_rule = ("[T-Pair]", [string_of_term e1; string_of_tipo t1; string_of_term e2; string_of_tipo t2; string_of_term e]) in
    let rules = rules1 @ rules2 @ [pair_rule] in
    (OrderedPair (t1, t2), rules)
  )

  (*  fst e   *)
  | Fst e -> (
    let (t, rules) = typeinfer e envtypes in (match t with
      | OrderedPair (t1, t2) -> (t1, rules @ [("T-Fst", [string_of_term e; string_of_term e])])
      | _ -> raise (TypeError "argument of `fst e` must be a pair")
    )
  )

  (*  snd e   *)
  | Snd e -> (
    let (t, rules) = typeinfer e envtypes in (match t with
      | OrderedPair (t1, t2) -> (t2, rules @ [("T-Snd", [string_of_term e; string_of_term e])])
      | _ -> raise (TypeError "argument of `snd e` must be a pair")
    )
  )

  (* if e1 then e2 else e3 *)
  | Conditional (e1, e2, e3) -> (
    let (t1, rules1) = typeinfer e1 envtypes in
    let (t2, rules2) = typeinfer e2 envtypes in
    let (t3, rules3) = typeinfer e3 envtypes in (
      if t1 <> Bool then raise (TypeError ("First argument of if must be boolean: " ^ string_of_tipo t1)) else ();
      if t2 <> t3 then raise (TypeError "Branches of if must have the same type") else ();
      let if_rule = ("[T-If]", [string_of_term e1; string_of_tipo t1; string_of_term e2; string_of_tipo t2; string_of_term e3; string_of_tipo t3; string_of_term e]) in
      let rules = rules1 @ rules2 @ rules3 @ [if_rule] in
      (t2, rules)
    )
  )

  (* x *)
  | Identifier x -> (
    match lookup x envtypes with
    | Some (e, t)   -> (t, [("T-Var", [x; string_of_tipo t])])
    | None          -> raise (TypeError ("Unbound identifier: " ^ x))
  )

  (* let x = e1 in e2 *)
  (* Γ ⊢ e1 : T, Γ[x |→ T'] ⊢ e2 : T' --> Γ ⊢ let x : T = e1 in e2 : T' *)
  | VarDefinition (x, t, e1, e2) -> (
    let (t1, rules1) = typeinfer e1 envtypes in
    let (t2, rules2) = typeinfer e2 ((x, e, t1)::envtypes) in
    let let_rule = ("[T-Let]", [string_of_term e1; string_of_tipo t1; x; string_of_term e2; string_of_tipo t2; string_of_term e]) in
    let rules = rules1 @ rules2 @ [let_rule] in
    (t2, rules)
  )

  | _ -> raise (TypeError "Malformed term")
);;

let typeof (e: term) = fst (typeinfer e []);;
let typeof (v: value) = typeof (term_of_value v);;


(* =============================================================
  Avaliação de Termos para L1
  ==============================================================  *)
let eval_rule_schema : (string * (string * string list)) list = [
    ("[E-Int]", ("n → n", ["n"]));
    ("[E-Bool]", ("b → b", ["b"]));
    
    ("[E-OrderedPair]", ("(v1, v2) → (v1, v2)", ["v1"; "v2"]));
    ("[E-Fst]", ("(v1, v2) → v1", ["v1"; "v2"]));
    ("[E-Snd]", ("(v1, v2) → v2", ["v1"; "v2"]));
  
    ("[E-Pair 1]", ("e1 → e1'   \t (e1, e2) → (e1', e2)", ["e1"; "e1'"; "e2"]));
    ("[E-Pair 2]", ("e2 → e2'   \t (v1, e2) → (v1, e2')", ["v1"; "e2"; "e2'"]));
  
    ("[E-If 1]", ("e1 → e1'   \t if e1 then e2 else e3 → if e1' then e2 else e3",
                  ["e1"; "e1'"; "e2"; "e3"]));
    ("[E-If 2]", ("e2 → e2'   \t if true then e2 else e3 → e2'",
                  ["e2"; "e2'"; "e3"]));
    ("[E-If 3]", ("e3 → e3'   \t if false then e2 else e3 → e3'",
                  ["e2"; "e3"; "e3'"]));
    
    ("[E-If True]", ("if true then e2 else e3 → e2", ["e2"; "e3"]));
    ("[E-If False]", ("if false then e2 else e3 → e3", ["e2"; "e3"]));

    ("[E-Var]", ("x = v → v", ["x"; "v"]));

    ("[E-Let 1]", ("e1 → e1'   \t let x = e1 in e2 → let x = e1' in e2", ["e1"; "e1'"; "e2"; "x"]));
    ("[E-Let 2]", ("let x:T = v in e2 --> {v/x}e2", ["v"; "e2"; "x"; "T"]));
  ]
;;

let rec eval (e: term) (envtypes: env): (value * env * ((string * string list) list)) = (
    match e with
    | Integer i   -> (VInt i,   envtypes,   [("E-Int", [string_of_int i])])
    | Boolean b   -> (VBool b,  envtypes,   [("E-Bool", [string_of_bool b])])
    | Pair (e1, e2) -> (
      let (v1, envtypes1, rules1) = eval e1 envtypes in
      let (v2, envtypes2, rules2) = eval e2 envtypes1 in
      (VPair (v1, v2), envtypes2, rules1 @ rules2 @ [("E-OrderedPair", [string_of_value v1; string_of_value v2])])
    )
  (* fst e *)
  | Fst e ->
    if not (is_value_term e) then
      (* Step inside e *)
      let (v, envtypes, rules) = eval e envtypes in
      let step = ("[E-Fst]", [string_of_term (Fst e);
                              string_of_term (Fst (term_of_value v))]) in
      (v, envtypes, step :: rules)
    else
      (match value_of_term e with
      | VPair (v1, v2) ->
          let step = ("[E-FstPair]", [string_of_term (Fst e);
                                      string_of_value v1]) in
          (v1, envtypes, [step])
      | _ -> failwith "fst applied to non-pair value"
      )

  (* snd e *)
  | Snd e ->
    if not (is_value_term e) then
      (* Step inside e *)
      let (v, envtypes, rules) = eval e envtypes in
      let step = ("[E-Snd]", [string_of_term (Snd e);
                              string_of_term (Snd (term_of_value v))]) in
      (v, envtypes, step :: rules)
    else
      (match value_of_term e with
      | VPair (v1, v2) ->
          let step = ("[E-SndPair]", [string_of_term (Snd e);
                                      string_of_value v2]) in
          (v2, envtypes, [step])
      | _ -> failwith "snd applied to non-pair value"
      )

    | Conditional (e1, e2, e3) -> (
        if not (is_value_term e1) then
          (* Step the guard e1 *)
          let (v1, envtypes, rules1) = eval e1 envtypes in
          let step = ("[E-If]", [string_of_term (Conditional (e1, e2, e3));
                                string_of_term (Conditional (term_of_value v1, e2, e3))]) in
          (v1, envtypes, step :: rules1)
    
        else match value_of_term e1 with
          | VBool true ->
              let (v2, envtypes, rules2) = eval e2 envtypes in
              (v2, envtypes, rules2 @
                [("[E-If True]", [string_of_term (Conditional (e1, e2, e3));
                                  string_of_value v2])])
    
          | VBool false ->
              let (v3, envtypes, rules3) = eval e3 envtypes in
              (v3, envtypes, rules3 @
                [("[E-If False]", [string_of_term (Conditional (e1, e2, e3));
                                    string_of_value v3])])
    
          | _ -> raise (TypeError
                          ("First argument of if must be boolean: "
                            ^ string_of_term e1
                            ^ " has type "
                            ^ string_of_tipo (typeof (value_of_term e1))))
    )

    (* x *)
    | Identifier x -> (
      match lookup x envtypes with
      | Some (e, t)   -> 
        let (v, envtypes, rules) = eval e envtypes in
        (v, envtypes, rules @ [("E-Var", [x; string_of_value v])])
      | None          -> raise (TypeError ("Unbound identifier: " ^ x)) 
    )

    (* let x = e1 in e2
       first, eval e1 up to a value v1, then substitute v1 for every occurence of x in e2 *)
    | VarDefinition (x, t, e1, e2) -> (
      let (v1, envtypes, rules1) = eval e1 envtypes in
      let (v2, envtypes, rules2) = eval e2 ((x, term_of_value v1, t)::envtypes) in
      (v2, envtypes,  rules1 @ 
                      rules2 @ 
                      [("E-Let 1", [string_of_term e1; string_of_value v1; string_of_term e2; x])] @
                      [("E-Let 2", [string_of_value v2; string_of_term e2; x; string_of_tipo t])])
    )

    | _ -> raise (TypeError "Malformed term")
  )

let print_rules title rules =
  print_endline ("-- " ^ title ^ " rules --");
    List.iter
      (fun (rule, terms) ->
        Printf.printf "%s\n\t%s\n" rule (String.concat "\n\t" terms))
          rules;
        print_endline ""
;;
      
let interpret (e: term) (env: env) : (value * env) = (
  let (t, rules)        = typeinfer e env in
  let (v, env', rules') = eval e env in
  print_endline ("expr: `" ^ string_of_term e ^ "`");
  print_endline ("type: `" ^ string_of_tipo t ^ "`");
  print_rules "Type inference" rules;
  print_rules "Evaluation" rules';
  (v, env')
)

let interpret (ex: term list) (env: env) : (value * env) list = List.map (fun e -> interpret e env) ex
;;


let _ = interpret ([
  VarDefinition ("x", Int, Integer 1,
    VarDefinition ("y", Int, Integer 2,
      Conditional(
        Fst(
          Pair(
            Boolean true,
            Identifier "x"
          )
        ),
        Identifier "x",
        Identifier "y"
      )))
]) ([]);;