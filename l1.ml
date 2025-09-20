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
  | OrderedPair     of term * term                  (* valor: par v,u           *)
  | Conditional     of term * term * term           (* if e1 then e2 else e3    *)

(*
  | BinaryOperation of binop  * term * term         (* e1 op e2                 *)
  | Identifier      of string                       (* x, identificador         *)
  | Application     of term * term                  (* e1 e2                    *)
  | Function        of string * tipo * term         (* fun x : t -> e           *)
  | VarDefinition   of string * tipo * term * term  (* let x : t = e1 in e2     *)
  | RecFunction     of string * tipo * term * term  (* let rec f : t = e1 in e2 *)
  | Fst             of term                         (* fst e                    *)
  | Snd             of term                         (* snd e                    *)
*)
;;

(*  repr. string de um termo em L1 *)
let rec string_of_term (e: term) : string = (match e with
    | Integer i -> string_of_int i
    | Boolean b -> string_of_bool b
    | OrderedPair (e1, e2) -> "(" ^ string_of_term e1 ^ ", " ^ string_of_term e2 ^ ")"
    | Conditional (e1, e2, e3) -> "if " ^ string_of_term e1 ^ " then " ^ string_of_term e2 ^ " else " ^ string_of_term e3
    | _ -> ""
  );;

(*  retorna verdadeiro se `e` é um valor em L1 *)
let rec is_value (e: term) : bool = (match e with
    | Integer _ -> true
    | Boolean _ -> true
    | OrderedPair (e1, e2) -> is_value e1 && is_value e2
    | _ -> false
  );;


(* --- ambiente de tipos ---------------------------------------- *)
type env = (string * tipo) list;;

(* busca por um identificador `x` no ambiente de tipos Γ e retorna o seu tipo `t`, se existir *)
let rec lookup (x: string) (envtypes: env) : tipo option = (
  match envtypes with
  | [] -> None
  | (y, t)::tl -> if x = y then Some t else lookup x tl 
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
  | OrderedPair (e1, e2) -> (
    let (t1, rules1) = typeinfer e1 envtypes in
    let (t2, rules2) = typeinfer e2 envtypes in
    let pair_rule = ("[T-Pair]", [string_of_term e1; string_of_tipo t1; string_of_term e2; string_of_tipo t2; string_of_term e]) in
    let rules = rules1 @ rules2 @ [pair_rule] in
    (OrderedPair (t1, t2), rules)
  )

  | Conditional (e1, e2, e3) -> (
    let (t1, rules1) = typeinfer e1 envtypes in
    let (t2, rules2) = typeinfer e2 envtypes in
    let (t3, rules3) = typeinfer e3 envtypes in (
      if t1 <> Bool then raise (TypeError "First argument of if must be boolean") else ();
      if t2 <> t3 then raise (TypeError "Branches of if must have the same type") else ();
      let if_rule = ("[T-If]", [string_of_term e1; string_of_tipo t1; string_of_term e2; string_of_tipo t2; string_of_term e3; string_of_tipo t3; string_of_term e]) in
      let rules = rules1 @ rules2 @ rules3 @ [if_rule] in
      (t2, rules)
    )
  )

  | _ -> raise (TypeError "Malformed term")
);;

let (tipo, rules) = typeinfer (OrderedPair (Integer 1, Boolean true)) [] in
List.iter (fun (rule, terms) -> Printf.printf "%s\n\t%s\n" rule (String.concat "\n\t" terms)) rules;
print_endline (string_of_term (OrderedPair (Integer 1, Boolean true)) ^ " : " ^ string_of_tipo tipo);;

let (tipo, rules) = typeinfer (Conditional (Boolean true, Integer 1, Integer 2)) [] in
List.iter (fun (rule, terms) -> Printf.printf "%s\n\t%s\n" rule (String.concat "\n\t" terms)) rules;
print_endline (string_of_term (Conditional (Boolean true, Integer 1, Integer 2)) ^ " : " ^ string_of_tipo tipo);;
