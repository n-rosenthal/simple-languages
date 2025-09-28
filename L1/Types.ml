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
  | OrderedPair (t1, t2) -> "(" ^ string_of_tipo t1 ^ ", " ^ string_of_tipo t2 ^ ")"
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

(*  dado um termo `e`, infere seu tipo `⊢ e : T` *)
let rec typeinfer (e: Terms.term) : (tipo, exn) result =
  match e with
  | Terms.Integer _ -> Ok Int
  | Terms.Boolean _ -> Ok Bool
  | Terms.OrderedPair (e1, e2) -> (match (typeinfer e1, typeinfer e2) with
    | (Ok t1, Ok t2) -> Ok (OrderedPair (t1, t2))
    | _ -> Error (TypeError ("typeinfer [OrderedPair]: " ^ Terms.string_of_term e, Some e))
  )

  | _ -> raise (TypeError ("typeinfer [_}: " ^ Terms.string_of_term e, Some e))
;;
