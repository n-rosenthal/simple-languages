(*
  Definição dos esquemas de regras para o sistema de tipos e a avaliação em L1
*)

(* esquema de regras *)
type schema = string * string list;;

(*
  ==============================================================
  Sistema de tipos para L1
  ==============================================================
*)
let rule_schemas_types : (string * schema) list = [
  ("T-Unit",
    ("Γ ⊢ (): unit", []));

  ("T-Int",
    ("Γ ⊢ n1: int", ["n1"; "Γ"]));

  ("T-Bool",
    ("Γ ⊢ b1: bool", ["b1"; "Γ"]));

  ("T-OrderedPair",
    ("Γ ⊢ e1: t1 ∧ Γ ⊢ e2: t2 ⊨ Γ ⊢ (e1, e2): (t1 * t2)",
    ["e1"; "t1"; "e2"; "t2"; "(e1, e2)"; "Γ"]));

  ("T-Fst",
    ("Γ ⊢ e: t1 * t2 ⊨ Γ ⊢ fst e: t1",
    ["e"; "t1"; "t2"; "Γ"]));

  ("T-Snd",
    ("Γ ⊢ e: t1 * t2 ⊨ Γ ⊢ snd e: t2",
    ["e"; "t1"; "t2"; "Γ"]));

  ("T-If",
    ("Γ ⊢ e1: bool ∧ Γ ⊢ e2: t ∧ Γ ⊢ e3: t ⊨ Γ ⊢ if e1 then e2 else e3: t",
    ["e1"; "e2"; "e3"; "t"; "Γ"]));

  ("T-Var",
    ("Γ('x') = t ⊨ x: t", ["x"; "t"; "Γ"]));

  ("T-Let",
    ("Γ ⊢ e1: t1, [(e1, x, t1) :: Γ] ⊢ e2: t2 ⊨ Γ ⊢ let x = e1 in e2: t2",
    ["e1"; "t1"; "x"; "e2"; "t2"; "Γ"]));

  ("T-Fun",
    ("Γ ⊢ e1: t1, Γ[x ⇒ t1] ⊢ e2: t2 ⊨ Γ ⊢ fun x = e1 [in e2]: t2",
    ["e1"; "t1"; "x"; "e2"; "t2"; "Γ"]));

];;

(* dada um esquema de regra e uma lista de (repr. string) de termos, retorna a regra correspondente *)
(* substitute placeholders ["e1"; "t1"; ...] in rule_body with given terms *)
let substitute ((rule_body, vars) : schema) (terms : string list) : string =
  let rec aux body vs ts =
    match vs, ts with
    | v::vs', t::ts' ->
        let re = Str.regexp_string (v) in
        let body' = Str.global_replace re (t) body in
        aux body' vs' ts'
    | [], [] -> body
    | _ -> raise (Invalid_argument "substitute")
  in
  aux rule_body vars terms
;;


let get_rule (name : string) : schema =
  List.assoc name rule_schemas_types
;;

let print_rule (name : string) (terms : string list) : unit =
  print_endline (substitute (get_rule name) terms)
;;