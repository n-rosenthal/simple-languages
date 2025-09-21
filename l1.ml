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


(* ============================================================
  L1_arith
  Extensão de L1 sobre operações aritméticas e relacionais sobre
  inteiros e booleanos
  
  1.  Criação do tipo `binop` para representar operações binárias
  2.  Extensão da sintaxe de termos
  3.  Inferência estática de tipos; avaliação; interpretador;
  =========================================================== *)

(* operadores binários sobre inteiros ou booleanos *)
type binop =
  | Add | Sub | Mul | Div                 (* op. bin. aritméticas *)
  | Eq  | Neq | Gt  | Geq | Lt  | Leq     (* op. bin. relacionais *)

let string_of_binop (b: binop) : string = match b with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq  -> "="
  | Neq -> "<>"
  | Gt  -> ">"
  | Geq -> ">="
  | Lt  -> "<"
  | Leq -> "<="
;;


(* 
  ==============================================================
  Sistema de tipos para L1
  ==============================================================
*)
type tipo =
  | Bool
  | Int
  | OrderedPair of tipo * tipo
  | Arrow       of tipo * tipo        (* tipo função*)
  | RecursiveFn of tipo               (* tipo função recursiva *)

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
  
  (* fun x : t = e1 [in e2] *)
  ("[T-Fun]",   ("Γ ⊢ e1 : t, Γ[x |→ t] ⊢ e2 : t' --> Γ ⊢ fun x : t = e1 [in e2] : t' ",
                ["e1"; "t1"; "x"; "e2"; "t2"; "fun x = e1 [in e2]"]));
  
  (* e1 e2 *)
  ("[T-App]",   ("Γ ⊢ e1 : t1 → t2   \t Γ ⊢ e2 : t1   \t Γ ⊢ e1 e2 : t2", 
                ["e1"; "t1"; "t2"; "e2"; "e1 e2"]));

  (* let rec *)
  ("[T-RecFn]", ("Γ, f : τ ⊢ e1 : τ   \t Γ, f : τ ⊢ e2 : τ2   \t Γ ⊢ let rec f : τ = e1 in e2 : τ2", 
                ["f"; "τ"; "e1"; "e2"; "let rec f = e1 in e2"]));

  (*  operações binárias    *)
  (*  op. bin. aritméticas  *)
  ("[T-Op+]",   ("Γ ⊢ e1 : int   \t Γ ⊢ e2 : int   \t Γ ⊢ e1 + e2 : int",
                ["e1"; "int"; "e2"; "int"; "e1 + e2"]));

  ("[T-Op-]",   ("Γ ⊢ e1 : int   \t Γ ⊢ e2 : int   \t Γ ⊢ e1 - e2 : int",
                ["e1"; "int"; "e2"; "int"; "e1 - e2"]));

  ("[T-Op*]",   ("Γ ⊢ e1 : int   \t Γ ⊢ e2 : int   \t Γ ⊢ e1 * e2 : int",
                ["e1"; "int"; "e2"; "int"; "e1 * e2"]));

  ("[T-Op/]",   ("Γ ⊢ e1 : int   \t Γ ⊢ e2 : int   \t Γ ⊢ e1 / e2 : int",
                ["e1"; "int"; "e2"; "int"; "e1 / e2"]));
  
  (*  op. bin. relacionais aritméticas  *)
  ("[T-Op<]",   ("Γ ⊢ e1 : int   \t Γ ⊢ e2 : int   \t Γ ⊢ e1 < e2 : bool",
                ["e1"; "int"; "e2"; "int"; "e1 < e2"]));

  ("[T-Op>]",   ("Γ ⊢ e1 : int   \t Γ ⊢ e2 : int   \t Γ ⊢ e1 > e2 : bool",
                ["e1"; "int"; "e2"; "int"; "e1 > e2"]));
  
  ("[T-Op<=]",  ("Γ ⊢ e1 : int   \t Γ ⊢ e2 : int   \t Γ ⊢ e1 <= e2 : bool",
                ["e1"; "int"; "e2"; "int"; "e1 <= e2"]));
  
  ("[T-Op>=]",  ("Γ ⊢ e1 : int   \t Γ ⊢ e2 : int   \t Γ ⊢ e1 >= e2 : bool",
                ["e1"; "int"; "e2"; "int"; "e1 >= e2"]));
  
  ("[T-Op=]",   ("Γ ⊢ e1 : int   \t Γ ⊢ e2 : int   \t Γ ⊢ e1 = e2 : bool",
                ["e1"; "int"; "e2"; "int"; "e1 = e2"]));
  
  ("[T-Op<>]",  ("Γ ⊢ e1 : int   \t Γ ⊢ e2 : int   \t Γ ⊢ e1 <> e2 : bool",
                ["e1"; "int"; "e2"; "int"; "e1 <> e2"]));

  (*  op. bin. relacionais booleanas  *)
  ("[T-Op&&]",  ("Γ ⊢ e1 : bool   \t Γ ⊢ e2 : bool   \t Γ ⊢ e1 && e2 : bool",
                ["e1"; "bool"; "e2"; "bool"; "e1 && e2"]));

  ("[T-Op||]",  ("Γ ⊢ e1 : bool   \t Γ ⊢ e2 : bool   \t Γ ⊢ e1 || e2 : bool",
                ["e1"; "bool"; "e2"; "bool"; "e1 || e2"]));
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
            | Function        of string * tipo * term * term  (* fun x : t -> e in e2     *)
            | Application     of term * term                  (* e1 e2                    *)
            | RecFunction     of string * tipo * term * term  (* let rec f : t = e1 in e2 *)
            | BinaryOperation of binop  * term * term         (* e1 op e2                 *)

(*
*)
;;

(* --- ambiente de tipos ---------------------------------------- *)
type env = (string * term * tipo) list;;

(* busca por um identificador `x` no ambiente de tipos Γ e retorna o seu tipo `t`, se existir *)
let rec lookup (x: string) (envtypes: env) : (term * tipo)  option = (
  match envtypes with
  | [] -> None
  | (y, e, t)::tl -> if x = y then Some (e, t) else lookup x tl 
);;

type value =
  | VInt      of int
  | VBool     of bool
  | VPair     of value * value
  | VClosure  of string * tipo * term * env (* fun x : t -> e *) (* valor função*)
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
    | Function (x, t, e, e2) -> "fun " ^ x ^ " : " ^ string_of_tipo t ^ " -> " ^ string_of_term e ^ " in " ^ string_of_term e2
    | Application (e1, e2) -> "( " ^ string_of_term e1 ^ " ) @ ( " ^ string_of_term e2 ^ " )"
    | RecFunction (f, t, e1, e2) -> "let rec " ^ f ^ " : " ^ string_of_tipo t ^ " = " ^ string_of_term e1 ^ " in " ^ string_of_term e2
    | BinaryOperation (op, e1, e2) -> string_of_term e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_term e2
  );;

let rec string_of_value (v: value) : string = (match v with
    | VInt i -> string_of_int i
    | VBool b -> string_of_bool b
    | VPair (v1, v2) -> "(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ")"
    | VClosure (x, t, e, env) -> "fun " ^ x ^ " : " ^ string_of_tipo t ^ " -> " ^ string_of_term e
    | VRecFn (f, e) -> "rec " ^ f ^ " -> " ^ string_of_term e
  );;


(* convert term -> value (only valid if term is a value) *)
  let rec value_of_term t =
    match t with
    | Integer n -> VInt n
    | Boolean b -> VBool b
    | Pair (a, b) -> VPair (value_of_term a, value_of_term b)
    | Function (x, t, e, e2) -> VClosure (x, t, e, [])
    | RecFunction (f, t, e1, e2) -> VRecFn (f, e1)
    | _ -> failwith ("value_of_term: not a value")
  
  (* convert value -> term *)
  let rec term_of_value v =
    match v with
    | VInt n -> Integer n
    | VBool b -> Boolean b
    | VPair (v1, v2) -> Pair (term_of_value v1, term_of_value v2)
    | VClosure (x, t, e, env) ->  e (* ? *)
    | VRecFn (f, e) -> e (* ? *)
    | _ -> failwith ("term_of_value: not a term")
  ;;
  
  (* is this term already a value? *)
  let rec is_value_term t =
    match t with
    | Integer _ | Boolean _  | Function _ | RecFunction _ -> true
    | Pair (a, b) -> is_value_term a && is_value_term b
    | _ -> false
  ;;
  





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

  (* fun x : t -> e *)
  | Function (x, t, body, e2) -> (
    let (t2, rules) = typeinfer body ((x, body, t) :: envtypes) in
    (Arrow (t, t2), ("[T-Fun]", [string_of_term (Function (x, t, body, e2));
                                  string_of_term body;
                                  string_of_tipo (Arrow (t, t2))]) :: rules)
  )

  (* e1 e2 *)
  | Application (e1, e2) -> (
    let (t_fun, rules') = typeinfer e1 envtypes in
    let (t_arg, rules'') = typeinfer e2 envtypes in
    (match t_fun with
      | Arrow (t', t'') -> (
          if t_arg = t' then
            (t'', rules' @ rules'' @ [("T-App", [string_of_term e1; string_of_term e2; string_of_term e])])
          else
            raise (TypeError ("Argument type does not match function type. Expected " ^ string_of_tipo t' ^ " but got " ^ string_of_tipo t_arg))
      )

      | _ -> raise (TypeError ("First argument of application must be a function, got " ^ string_of_term e1))
    )
  )

  (* let rec *)
  | RecFunction (f, t, e1, e2) -> (
    let (t1, rules1) = typeinfer e1 ((f, e1, t) :: envtypes) in
    if t1 <> t then
      raise (TypeError ("Recursive definition type mismatch: " ^
                        string_of_tipo t ^ " expected, got " ^
                        string_of_tipo t1));
    let (t2, rules2) = typeinfer e2 ((f, e2, t) :: envtypes) in
    (t2,
    ("[T-LetRec]", [string_of_term (RecFunction (f, t, e1, e2));
                    string_of_tipo t2]) :: rules1 @ rules2)
  )
  
  
  (*  operações binárias  *)
  | BinaryOperation (op, e1, e2) -> (
    let (t1, rules1) = typeinfer e1 envtypes in
    let (t2, rules2) = typeinfer e2 envtypes in
    (match (op, t1, t2) with
      (* op. binárias aritméticas *)
      | (Add, Int, Int) -> (Int, rules1 @ rules2 @ [("T-Op+", [string_of_term e1; string_of_term e2; string_of_term e])])
      | (Sub, Int, Int) -> (Int, rules1 @ rules2 @ [("T-Op-", [string_of_term e1; string_of_term e2; string_of_term e])])
      | (Mul, Int, Int) -> (Int, rules1 @ rules2 @ [("T-Op*", [string_of_term e1; string_of_term e2; string_of_term e])])
      | (Div, Int, Int) -> (Int, rules1 @ rules2 @ [("T-Op/", [string_of_term e1; string_of_term e2; string_of_term e])])

      (* op. binárias relacionais aritméticas *)
      | (Eq, Int, Int) -> (Bool, rules1 @ rules2 @ [("T-Op=", [string_of_term e1; string_of_term e2; string_of_term e])])
      | (Lt, Int, Int) -> (Bool, rules1 @ rules2 @ [("T-Op<", [string_of_term e1; string_of_term e2; string_of_term e])])
      | (Gt, Int, Int) -> (Bool, rules1 @ rules2 @ [("T-Op>", [string_of_term e1; string_of_term e2; string_of_term e])])
      | (Leq, Int, Int) -> (Bool, rules1 @ rules2 @ [("T-Op<=", [string_of_term e1; string_of_term e2; string_of_term e])])
      | (Geq, Int, Int) -> (Bool, rules1 @ rules2 @ [("T-Op>=", [string_of_term e1; string_of_term e2; string_of_term e])])

      (* op. binárias relacionais booleanas *)
      | (Eq, Bool, Bool) -> (Bool, rules1 @ rules2 @ [("T-Op&&", [string_of_term e1; string_of_term e2; string_of_term e])])
      | (Neq, Bool, Bool) -> (Bool, rules1 @ rules2 @ [("T-Op||", [string_of_term e1; string_of_term e2; string_of_term e])])

      (*  erro  *)
      | _ -> raise (TypeError ("Invalid binary operation: " ^ string_of_term e))
    )
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

    ("[E-Fun]", ("e1 → e1'   \t fun x -> e1 → fun x -> e1'", ["e1"; "e1'"]));

    ("[E-App 1]", ("e1 → e1'   \t e1 e2 → e1' e2", ["e1"; "e1'"; "e2"]));
    ("[E-App 2]", ("e2 → e2'   \t (v1 e2) → (v1 e2')", ["e2"; "e2'"; "v1"]));
    ("[E-AppFun]", ("e1 = fun x : t -> e   \t e2 = v2   \t (fun x : t -> e) v2 → e[x ↦ v2]", ["e1"; "e2"; "t"; "x"]));

    ("[E-RecFun]", ("e1 = fun x : t -> e   \t e2 = v2   \t let rec x = e1 in e2 → let rec x = e1 in e2", ["e1"; "e2"; "t"; "x"]));

    ("[E-Binop 1]", ("e1 → e1'   \t e1 op e2 → e1' op e2", ["e1"; "e1'"; "e2"; "op"]));
    ("[E-Binop 2]", ("e2 → e2'   \t e1 op e2 → e1 op e2'", ["e1"; "e2"; "e2'"; "op"]));
    ("[E-Op+]",     ("[[e1]] = v1   \t [[e2]] = v2   \t [[e1 + e2]] → v", ["e1"; "v1"; "e2"; "v2"; "v";]));
    ("[E-Op-]",     ("[[e1]] = v1   \t [[e2]] = v2   \t [[e1 - e2]] → v", ["e1"; "v1"; "e2"; "v2"; "v";]));
    ("[E-Op*]",     ("[[e1]] = v1   \t [[e2]] = v2   \t [[e1 * e2]] → v", ["e1"; "v1"; "e2"; "v2"; "v";]));
    ("[E-Op/]",     ("[[e1]] = v1   \t [[e2]] = v2   \t v2 <> 0   \t [[e1 / e2]] → v", ["e1"; "v1"; "e2"; "v2"; "v";]));
    ("[E-Op=]",     ("[[e1]] = v1   \t [[e2]] = v2   \t v2 <> 0   \t [[e1 = e2]] → v", ["e1"; "v1"; "e2"; "v2"; "v";]));
    ("[E-Op<>]",    ("[[e1]] = v1   \t [[e2]] = v2   \t v2 <> 0   \t [[e1 <> e2]] → v", ["e1"; "v1"; "e2"; "v2"; "v";]));
    ("[E-Op<]",     ("[[e1]] = v1   \t [[e2]] = v2   \t v2 <> 0   \t [[e1 < e2]] → v", ["e1"; "v1"; "e2"; "v2"; "v";]));
    ("[E-Op>]",     ("[[e1]] = v1   \t [[e2]] = v2   \t v2 <> 0   \t [[e1 > e2]] → v", ["e1"; "v1"; "e2"; "v2"; "v";]));
    ("[E-Op>=]",    ("[[e1]] = v1   \t [[e2]] = v2   \t v2 <> 0   \t [[e1 >= e2]] → v", ["e1"; "v1"; "e2"; "v2"; "v";]));
    ("[E-Op<=]",    ("[[e1]] = v1   \t [[e2]] = v2   \t v2 <> 0   \t [[e1 <= e2]] → v", ["e1"; "v1"; "e2"; "v2"; "v";]));
    ("[E-Op&&]",    ("[[e1]] = v1   \t [[e2]] = v2   \t [[e1 && e2]] → v", ["e1"; "v1"; "e2"; "v2"; "v";]));
    ("[E-Op||]",    ("[[e1]] = v1   \t [[e2]] = v2   \t [[e1 || e2]] → v", ["e1"; "v1"; "e2"; "v2"; "v";]));
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

    (* fun x : t -> e *)
    | Function (x, t, e, e2) -> (
        (VClosure (x, t, e, envtypes), envtypes, [("E-Fun", [x; string_of_tipo t; string_of_term e])])
    )

    (* e1 e2 *)
    | Application (e1, e2) -> (
      if not (is_value_term e1) then
        let (v1, env1, rules1) = eval e1 envtypes in
        let step = ("[E-App1]", [string_of_term (Application (e1, e2));
                                string_of_term (Application (term_of_value v1, e2))]) in
        (v1, env1, step :: rules1)
  
      else if not (is_value_term e2) then
        let (v2, env2, rules2) = eval e2 envtypes in
        let step = ("[E-App2]", [string_of_term (Application (e1, e2));
                                string_of_term (Application (e1, term_of_value v2))]) in
        (v2, env2, step :: rules2)
  
      else
        match value_of_term e1 with
        | VClosure (x, t, body, closure_env) ->
            let v2 = value_of_term e2 in
            let new_env = (x, term_of_value v2, t) :: closure_env in
            let (v, env', rules) = eval body new_env in
            let step = ("[E-AppFun]", [string_of_term (Application (e1, e2));
                                      string_of_value v]) in
            (v, env', step :: rules)
  
        | _ -> raise (TypeError
                        ("First argument of application must be a function: "
                          ^ string_of_term e1
                          ^ " has type "
                          ^ string_of_tipo (typeof (value_of_term e1))))
    )
    
  | RecFunction (f, t, e1, e2) -> (
      (* create recursive closure: f is bound to itself *)
      let rec_closure = VClosure (f, t, e1, envtypes) in
      let new_env = (f, term_of_value rec_closure, t) :: envtypes in
      let (v, env', rules) = eval e2 new_env in
      let step = ("[E-LetRec]",
                  [string_of_term (RecFunction (f, t, e1, e2));
                  string_of_value v]) in
      (v, env', step :: rules)
  )

  | BinaryOperation (op, e1, e2) -> (
    let (t1, rules1) = typeinfer e1 envtypes in
    let (t2, rules2) = typeinfer e2 envtypes in
    let (v1, envtypes1, rules3) = eval e1 envtypes in
    let (v2, envtypes2, rules4) = eval e2 envtypes1 in (match (op, v1, v2) with
      (* op. binárias aritméticas *)
      | (Add, VInt i1, VInt i2) -> (VInt (i1 + i2), envtypes2, rules1 @ rules2 @ rules3 @ rules4 @ [("E-Add", [string_of_value v1; string_of_value v2])])
      | (Sub, VInt i1, VInt i2) -> (VInt (i1 - i2), envtypes2, rules1 @ rules2 @ rules3 @ rules4 @ [("E-Sub", [string_of_value v1; string_of_value v2])])
      | (Mul, VInt i1, VInt i2) -> (VInt (i1 * i2), envtypes2, rules1 @ rules2 @ rules3 @ rules4 @ [("E-Mul", [string_of_value v1; string_of_value v2])])
      | (Div, VInt i1, VInt i2) -> 
          if i2 = 0 then raise (TypeError "Dividing by zero") else (VInt (i1 / i2), envtypes2, rules1 @ rules2 @ rules3 @ rules4 @ [("E-Div", [string_of_value v1; string_of_value v2])])
      
      (* op. binárias relacionais aritméticas *)
      | (Eq, VInt i1, VInt i2) -> (VBool (i1 = i2), envtypes2, rules1 @ rules2 @ rules3 @ rules4 @ [("E-Eq", [string_of_value v1; string_of_value v2])])
      | (Neq, VInt i1, VInt i2) -> (VBool (i1 <> i2), envtypes2, rules1 @ rules2 @ rules3 @ rules4 @ [("E-Neq", [string_of_value v1; string_of_value v2])])
      | (Lt, VInt i1, VInt i2) -> (VBool (i1 < i2), envtypes2, rules1 @ rules2 @ rules3 @ rules4 @ [("E-Lt", [string_of_value v1; string_of_value v2])])
      | (Gt, VInt i1, VInt i2) -> (VBool (i1 > i2), envtypes2, rules1 @ rules2 @ rules3 @ rules4 @ [("E-Gt", [string_of_value v1; string_of_value v2])])
      | (Leq, VInt i1, VInt i2) -> (VBool (i1 <= i2), envtypes2, rules1 @ rules2 @ rules3 @ rules4 @ [("E-Leq", [string_of_value v1; string_of_value v2])])
      | (Geq, VInt i1, VInt i2) -> (VBool (i1 >= i2), envtypes2, rules1 @ rules2 @ rules3 @ rules4 @ [("E-Geq", [string_of_value v1; string_of_value v2])])

      (* op. binárias relacionais booleanas *)
      | (Eq, VBool b1, VBool b2) -> (VBool (b1 = b2), envtypes2, rules1 @ rules2 @ rules3 @ rules4 @ [("E-Eq", [string_of_value v1; string_of_value v2])])
      | (Neq, VBool b1, VBool b2) -> (VBool (b1 <> b2), envtypes2, rules1 @ rules2 @ rules3 @ rules4 @ [("E-Neq", [string_of_value v1; string_of_value v2])])

      (*  erro  *)
      | _ -> raise (TypeError "Malformed term")
      )
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
  print_endline "==="; print_endline ("value: `" ^ string_of_value v ^ "`"); print_endline "===";
  (v, env')
)

let interpret (ex: term list) (env: env) : (value * env) list = List.map (fun e -> interpret e env) ex
;;


let _ = interpret ([
  (*  sucessor function *)
  Application (Function ("x", Int, BinaryOperation(Add, Identifier "x", Integer 1), Identifier "x"), Integer 5);
]) ([]);;