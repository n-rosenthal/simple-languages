(*
  Sistema de tipos e inferência estática de tipos para L1
*)

(*  tipos de L1 *)
type tipo =
  | Bool
  | Int
  | OrderedPair of tipo * tipo
;;

(* repr. string de um tipo *)
let rec string_of_tipo (t: tipo) : string = match t with
  | Bool -> "bool"
  | Int -> "int"
  | OrderedPair (t1, t2) -> "(" ^ string_of_tipo t1 ^ " * " ^ string_of_tipo t2 ^ ")"
;;


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
let rec typeinfer (e: Terms.term) : (tipo * Repr.schema list, exn) result =
  match e with
  (* Integer n *)
  | Terms.Integer n -> Ok (Int, [("T-Int", [Terms.string_of_term e])])

  (* Boolean b *)
  | Terms.Boolean b -> Ok (Bool, [("T-Bool", [Terms.string_of_term e])])

  (* (e1, e2) *)
  | Terms.OrderedPair (e1, e2) -> (
    match (typeinfer e1, typeinfer e2) with
    | (Ok (t1, rules1), Ok (t2, rules2)) -> Ok (OrderedPair (t1, t2), rules1 @ rules2)
    | (Ok (t1, rules1), Error e) -> Error e
    | (Error e, Ok (t2, rules2)) -> Error e
    | (Error e, Error e') -> Error e
  )

  (* fst e *)
  | Terms.Fst e -> (
    match typeinfer e with
    | Ok (t, rules) -> (
      match t with
      | OrderedPair (t1, t2) -> Ok (t1, rules @ [("T-Fst", [Terms.string_of_term e; string_of_tipo t1; string_of_tipo t2])])
      | _ -> Error (TypeError ("argument of `fst e` must be a pair", Some e))
    )
    | Error e -> Error e
  )

  (* snd e *)
  | Terms.Snd e -> (
    match typeinfer e with
    | Ok (t, rules) -> (
      match t with
      | OrderedPair (t1, t2) -> Ok (t2, rules @ [("T-Snd", [Terms.string_of_term e; string_of_tipo t1; string_of_tipo t2])])
      | _ -> Error (TypeError ("argument of `snd e` must be a pair", Some e))
    )
    | Error e -> Error e
  )

  | _ -> Error (TypeError ("typeinfer failed", Some e))
;;
