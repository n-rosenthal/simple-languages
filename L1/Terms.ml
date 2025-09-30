(**
  Definições da sintaxe da linguagem L1:
  1.  termos (expressões)

  2.  valores


*)



(*  sintaxe de termos sobre L1 *)
type term =
  | None                                    (* () *)
  | Integer of int                          (* n *)
  | Boolean of bool                         (* b *)
  | OrderedPair of term * term              (* (e1, e2) *)
  | Fst of term                             (* fst e *)
  | Snd of term                             (* snd e *)
  | Conditional of term * term * term       (* if e1 then e2 else e3 *)
  | Identifier of string                    (* x, identificador, var *)
  | VarDefinition of binding * term         (* let x = e1 in e2 *)
  | Function of binding * term              (* fun x -> e1 in e2 *)
  | Application of term * term              (* e1 e2 *)

  (* extension: exception and error handling *)
  | TryWith of term * term                  (* try e1 with e2 *)
  | Raise of term                           (* raise e1 *)
  | TypeError of string                     (* type error *)
and binding = term * string;;               (*  name binding, associação de um termo a um nome *)

let rec string_of_term (e: term) : string =
  match e with
  | None -> "()"
  | Integer n -> string_of_int n
  | Boolean b -> string_of_bool b
  | OrderedPair (e1, e2) -> "(" ^ string_of_term e1 ^ ", " ^ string_of_term e2 ^ ")"
  | Fst e -> "fst " ^ string_of_term e
  | Snd e -> "snd " ^ string_of_term e
  | Conditional (e1, e2, e3) -> "if " ^ string_of_term e1 ^ " then " ^ string_of_term e2 ^ " else " ^ string_of_term e3
  | Identifier x -> x
  | VarDefinition (b, e) -> "let " ^ string_of_binding b ^ " in " ^ string_of_term e
  | Function (b, e2) -> "fun " ^ string_of_binding b ^ " in " ^ string_of_term e2
  | Application (e1, e2) -> string_of_term e1 ^ " @ " ^ string_of_term e2

  | TryWith (e1, e2) -> "try " ^ string_of_term e1 ^ " with " ^ string_of_term e2
  | Raise e -> "raise " ^ string_of_term e
  | TypeError s -> "type error: " ^ s
  | _ -> raise (Invalid_argument "string_of_term");
    and string_of_binding (b: binding) : string =
      let (e, x) = b in
      "'" ^ x ^ "' = " ^ string_of_term e
;;

let rec ast_of_term (e: term) : string =
  match e with
  | None -> "(None)"
  | Integer n -> "(Integer " ^ string_of_int n ^ ")"
  | Boolean b -> "(Boolean " ^ string_of_bool b ^ ")"
  | OrderedPair (e1, e2) -> "(OrderedPair (" ^ ast_of_term e1 ^ ", " ^ ast_of_term e2 ^ "))"
  | Fst e -> "(Fst (" ^ ast_of_term e ^ "))"
  | Snd e -> "(Snd (" ^ ast_of_term e ^ "))"
  | Conditional (e1, e2, e3) -> "(Conditional (" ^ ast_of_term e1 ^ ", " ^ ast_of_term e2 ^ ", " ^ ast_of_term e3 ^ "))"
  | Identifier x -> "(Identifier \"" ^ x ^ "\")"
  | VarDefinition (b, e) -> "(VarDefinition (" ^ ast_of_binding b ^ ", " ^ ast_of_term e ^ "))"
  | Function (b, e2) -> "(Function (" ^ ast_of_binding b ^ ", " ^ ast_of_term e2 ^ "))"
  | Application (e1, e2) -> "(Application (" ^ ast_of_term e1 ^ ", " ^ ast_of_term e2 ^ "))"

  | TryWith (e1, e2) -> "(TryWith (" ^ ast_of_term e1 ^ ", " ^ ast_of_term e2 ^ "))"
  | Raise e -> "(Raise (" ^ ast_of_term e ^ "))"
  | TypeError s -> "(TypeError \"" ^ s ^ "\")"
  | _ -> raise (Invalid_argument "string_of_term");
    and ast_of_binding (b: binding) : string =
      let (e, x) = b in
      "(" ^ ast_of_term e ^ ", \"" ^ x ^ "\")"
;;



let rec size (e: term) : int =
  match e with
  | None -> 1
  | Integer _ -> 1
  | Boolean _ -> 1
  | OrderedPair (e1, e2) -> size e1 + size e2
  | Fst e -> (match e with
              | OrderedPair (e1, e2) -> size e1
              | _ -> 0)
  | Snd e -> (match e with
              | OrderedPair (e1, e2) -> size e2
              | _ -> 0)
  | Conditional (e1, e2, e3) -> size e1 + size e2 + size e3 + 1
  | Identifier _ -> 1
  | VarDefinition (b, e) -> (size (fst b) + size e)
  | Function (b, e2) -> (size (fst b) + size e2 + 1)
  | Application (e1, e2) -> size e1 + size e2

  | TryWith (e1, e2) -> size e1 + size e2 + 1
  | Raise e -> size e + 1
  | TypeError _ -> 1
;;

let rec depth (e: term) : int =
  match e with
  | None -> 1
  | Integer _ -> 1
  | Boolean _ -> 1
  | OrderedPair (e1, e2) -> max (depth e1) (depth e2) + 1
  | Fst e -> (match e with
              | OrderedPair (e1, e2) -> depth e1
              | _ -> 0)
  | Snd e -> (match e with
              | OrderedPair (e1, e2) -> depth e2
              | _ -> 0)
  | Conditional (e1, e2, e3) -> max (depth e1) (max (depth e2) (depth e3)) + 1
  | Identifier _ -> 1
  | VarDefinition (b, e) -> max (depth (fst b)) (depth e)
  | Function (b, e2) -> max (depth (fst b)) (depth e2) + 1
  | Application (e1, e2) -> max (depth e1) (depth e2)

  | TryWith (e1, e2) -> max (depth e1) (depth e2) + 1
  | Raise e -> depth e + 1
  | TypeError _ -> 1
;;

(* constantes presentes em `e` *)
let rec constants (e: term) : string list =
  match e with
  | None -> []
  | Integer _ -> []
  | Boolean _ -> []
  | OrderedPair (e1, e2) -> constants e1 @ constants e2
  | Fst e -> constants e
  | Snd e -> constants e
  | Conditional (e1, e2, e3) -> constants e1 @ constants e2 @ constants e3
  | Identifier x -> [x]
  | VarDefinition (b, e) -> constants (fst b) @ constants e
  | Function (b, e2) -> constants (fst b) @ constants e2
  | Application (e1, e2) -> constants e1 @ constants e2

  | TryWith (e1, e2) -> constants e1 @ constants e2
  | Raise e -> constants e
  | TypeError _ -> []
;;





(*  sintaxe de valores sobre L1 *)
type value =
  | Unit                                    (* (), the value of None or Unit *)
  | VInt of int                             (* n *)
  | VBool of bool                           (* b *)
  | VPair of value * value                  (* (v1, v2) *)
  | Closure of binding * term               (* fun x -> e1 in e2 *)

  | Error of string                         (* error is the value of a term Exception x *)
;;

let rec string_of_value (v: value) : string =
  match v with
  | Unit -> "()"
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b
  | VPair (v1, v2) -> "(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ")"
  | Closure (b, e) -> "fun " ^ string_of_binding b ^ " -> " ^ string_of_term e
  | Error s -> "error: " ^ s
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
  | None -> true
  | Integer _ -> true
  | Boolean _ -> true
  | OrderedPair (e1, e2) -> is_value e1 && is_value e2
  | _ -> false
;;

(*  dado um termo `e`, se `e` for um valor, então retorna seu valor correspondente em L1 *)
let rec value_of_term (e: term) : value option =
  if (not (is_value e)) then None
  else match e with
    | None -> Some (Unit)
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

    | Fst e -> (match e with
                | OrderedPair (e1, e2) -> value_of_term e1
                | _ -> None)
    | Snd e -> (match e with
                | OrderedPair (e1, e2) -> value_of_term e2
                | _ -> None)

    | Conditional (e1, e2, e3) -> (
      match value_of_term e1 with
      | Some (VBool b) -> if b then value_of_term e2 else value_of_term e3
      | Some _ -> None
      | None -> None
    )

    | Identifier _ -> None

    (* let x = e1 in e2, onde `nb = (x, e1)`*)
    | VarDefinition (nb, e) -> (
      match value_of_term (fst nb) with
      | Some _ -> value_of_term e
      | None -> None
    )

    | TryWith (e1, e2) -> (
      match value_of_term e1 with
      | Some _ -> value_of_term e1
      | None -> value_of_term e2
    )

    | Raise e -> (
      match value_of_term e with
      | Some (Error s) -> Some (Error s)
      | Some _ -> None
      | None -> None
    )

    | _ -> raise (ValueError ("value_of_term: not a value", Some e))
;;

(* dado um termo `e`, retorna verdadeiro se `e` for um valor numérico em L1 *)
let rec is_numeric (e : term) : bool =
  match e with
  | Integer _ -> true
  | _ -> false
;;

