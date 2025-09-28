(**
  Definições da sintaxe da linguagem L1:
  1.  termos (expressões)

  2.  valores


*)



(*  sintaxe de termos sobre L1 *)
type term =
  | Integer of int                          (* n *)
  | Boolean of bool                         (* b *)
  | OrderedPair of term * term              (* (e1, e2) *)
;;

(*  repr. string de um termo `e` *)
let rec string_of_term (e: term) : string =
  match e with
  | Integer n -> string_of_int n
  | Boolean b -> string_of_bool b
  | OrderedPair (e1, e2) -> "(" ^ string_of_term e1 ^ ", " ^ string_of_term e2 ^ ")"
;;


let rec size (e: term) : int =
  match e with
  | Integer _ -> 1
  | Boolean _ -> 1
  | OrderedPair (e1, e2) -> size e1 + size e2
;;

let rec depth (e: term) : int =
  match e with
  | Integer _ -> 1
  | Boolean _ -> 1
  | OrderedPair (e1, e2) -> max (depth e1) (depth e2) + 1
;;

let rec constants (e : term) : string list =
  match e with
  | Integer _ | Boolean _ -> [string_of_term e]
  | OrderedPair (e1, e2) -> constants e1 @ constants e2
;;



(*  sintaxe de valores sobre L1 *)
type value =
  | VInt of int                             (* n *)
  | VBool of bool                           (* b *)
  | VPair of value * value                  (* (v1, v2) *)
;;

(*  repr. string de um valor `v` *)
let rec string_of_value (v: value) : string =
  match v with
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b
  | VPair (v1, v2) -> "(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ")"
;;

(*  ValueError ::= erro na avaliação de um termo enquanto valor *)
exception ValueError of string * term option;;

(* repr. string de uma exceção *)
let string_of_value_error (e: exn) : string =
  match e with
  | ValueError (msg, Some t) -> msg ^ ": " ^ string_of_term t
  | ValueError (msg, None) -> msg
  | _ -> raise e
;;

(*  dado um termo `e`, retorna verdadeiro se `e` for um valor em L1 *)
let rec is_value (e: term) : bool =
  match e with
  | Integer _ -> true
  | Boolean _ -> true
  | OrderedPair (e1, e2) -> is_value e1 && is_value e2
;;

(*  dado um termo `e`, se `e` for um valor, então retorna seu valor correspondente em L1 *)
let rec value_of_term (e: term) : value option =
  if (not (is_value e)) then None
  else match e with
    | Integer n -> Some (VInt n)
    | Boolean b -> Some (VBool b)
    | OrderedPair (e1, e2) -> (
      match (value_of_term e1, value_of_term e2) with
      | (Some v1, Some v2) -> Some (VPair (v1, v2))
      | (Some v1, None)    -> raise (
          ValueError ("value_of_term [OrderedPair]: second argument is not a value" ^ string_of_term e2, Some e2))
      | (None, Some v2)    -> raise (
          ValueError ("value_of_term [OrderedPair]: first argument is not a value" ^ string_of_term e1, Some e1))
      | (None, None)       -> None
      )

    | _ -> raise (ValueError ("value_of_term: not a value", Some e))
;;

(* dado um termo `e`, retorna verdadeiro se `e` for um valor numérico em L1 *)
let rec is_numeric (e : term) : bool =
  match e with
  | Integer _ -> true
  | _ -> false
;;

