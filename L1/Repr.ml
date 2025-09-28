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
  ("T-Int",
    ("Γ ⊢ n : int", ["n"]));

  ("T-Bool",
    ("Γ ⊢ b : bool", ["b"]));

  ("T-OrderedPair",
    ("Γ ⊢ e1 : t1, Γ ⊢ e2 : t2 => Γ ⊢ (e1, e2) : (t1, t2)",
    ["e1"; "t1"; "e2"; "t2"]));
];;

(* dada um esquema de regra e uma lista de (repr. string) de termos, retorna a regra correspondente *)
(* substitute placeholders ["e1"; "t1"; ...] in rule_body with given terms *)
let substitute ((rule_body, vars) : schema) (terms : string list) : string =
  let rec aux body vs ts =
    match vs, ts with
    | v::vs', t::ts' ->
        let re = Str.regexp_string (" " ^ v ^ " ") in
        let body' = Str.global_replace re (" " ^ t ^ " ") body in
        aux body' vs' ts'
    | [], [] -> body
    | _ -> failwith "substitute: mismatch between variables and terms"
  in
  aux rule_body vars terms
;;


let get_rule (name : string) : schema =
  List.assoc name rule_schemas_types
;;

let print_rule (name : string) (terms : string list) : unit =
  print_endline (substitute (get_rule name) terms)
;;