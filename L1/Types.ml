(*
  Sistema de tipos e inferência estática de tipos para L1
*)

(*  tipos de L1 *)
type tipo =
  | NoneType
  | Bool
  | Int
  | Pair of tipo * tipo
  | ExceptionType of error
  | Arrow of tipo * tipo
and error =
    | TypeError
    | RuntimeError
    | ProgrammingError
    | UnboundIdentifier of string
    | NotImplemented of string
;;


(* repr. string de um tipo *)
let rec string_of_tipo (t: tipo) : string = match t with
  | NoneType -> "None"
  | Bool -> "bool"
  | Int -> "int"
  | Pair (t1, t2) -> "(" ^ string_of_tipo t1 ^ " * " ^ string_of_tipo t2 ^ ")"
  | ExceptionType exn -> ("exn: " ^ string_of_error exn)
  | Arrow (t1, t2) -> "(" ^ string_of_tipo t1 ^ " -> " ^ string_of_tipo t2 ^ ")"
and string_of_error (e: error) : string = match e with
  | TypeError -> "TypeError"
  | RuntimeError -> "RuntimeError"
  | ProgrammingError -> "ProgrammingError"
  | UnboundIdentifier s -> "UnboundIdentifier " ^ s
  | NotImplemented s -> "NotImplemented" ^ s 
;;