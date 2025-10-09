(**
  Sintaxes de termos e de valores para a linguagem L2

version 1.0
*)

(* tipo `term` é um tipo extensível *)
type term = ..
and binary_operator =
  (* op. binários aritméticos *)
  | Sum | Sub | Mul | Div
  (* op. binários relacionais *)
  | Eq  | Neq | Gt  | Geq | Lt  | Leq
  (* op. binários lógicos *)
  | And | Or
and memory_position = int
;;

(* expressões de L2*)
type term +=
  | Nothing                                           (* (), a expressão de tipo Unit/VoidType *)
  | Integer of int                                    (* int n *)
  | Boolean of bool                                   (* bool b *)
  | BinaryOperation of binary_operator * term * term  (* e1 op e2*)
  | Conditional of term * term * term                 (* if e1 then e2 else e3 *)
  | Identifier of string                              (* x, identificador, var *)
  | VarDefinition of string * term * Types.tipo * term      (* let x : t = e1 in e2 *)
  | Assignment of term * term                         (* e1 := e2 *)
  | Dereference of term                               (* !e *)
  | Allocation of term                                (* new e *)
  | While of term * term                              (* while e1 do e2 *)
  | Sequence of term * term                           (* e1; e2 *)
  | Location of memory_position                       (* l *)
  | Read                                              (* read () *)
  | Print of term                                     (* print e *)

  (* erro *)
  | RuntimeError of string


(* repr. string de um operador binário *)
let string_of_binop (b: binary_operator) : string = match b with
  | Sum -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq  -> "="
  | Neq -> "<>"
  | Gt  -> ">"
  | Geq -> ">="
  | Lt  -> "<"
  | Leq -> "<="
  | And -> "&&"
  | Or  -> "||"
;;

(* repr. string de um termo *)
let rec string_of_term (e: term) : string = match e with
  | Nothing -> "()"
  | Integer n -> string_of_int n
  | Boolean b -> string_of_bool b
  | BinaryOperation (op, e1, e2) -> string_of_term e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_term e2
  | Conditional (e1, e2, e3) -> "if " ^ string_of_term e1 ^ " then " ^ string_of_term e2 ^ " else " ^ string_of_term e3
  | Identifier x -> x
  | VarDefinition (x, e1, t, e2) -> "let " ^ x ^ " : " ^ Types.string_of_tipo t ^ " = " ^ string_of_term e1 ^ " in " ^ string_of_term e2
  | Assignment (e1, e2) -> string_of_term e1 ^ " := " ^ string_of_term e2
  | Dereference e -> "!" ^ string_of_term e
  | Allocation e -> "new " ^ string_of_term e

  | RuntimeError s -> "RuntimeError: " ^ s

  | _
;;

(* repr. string da árvore de sintaxe abstrata (concreta, ?) de um termo *)
let rec ast_of_term (e: term) : string = (match e with
  | Nothing -> "(Unit)"
  | Integer n -> "(Integer " ^ string_of_int n ^ ")"
  | Boolean b -> "(Boolean " ^ string_of_bool b ^ ")"
  | BinaryOperation (op, e1, e2) -> "(BinaryOperation (" ^ string_of_binop op ^ ", " ^ ast_of_term e1 ^ ", " ^ ast_of_term e2 ^ "))"
  | Conditional (e1, e2, e3) -> "(Conditional (" ^ ast_of_term e1 ^ ", " ^ ast_of_term e2 ^ ", " ^ ast_of_term e3 ^ "))"
  | Identifier x -> "(Identifier " ^ x ^ ")"
  | VarDefinition (x, e1, t, e2) -> "(VarDefinition (" ^ x ^ ", " ^ ast_of_term e1 ^ ", " ^ Types.string_of_tipo t ^ ", " ^ ast_of_term e2 ^ "))"
  | Assignment (e1, e2) -> "(Assignment (" ^ ast_of_term e1 ^ " := " ^ ast_of_term e2 ^ "))"
  | Dereference e -> "(Dereference !(" ^ ast_of_term e ^ "))"
  | Allocation e -> "(Allocation new (" ^ ast_of_term e ^ "))"

  | RuntimeError s -> "(RuntimeError '" ^ s ^ "')"

  | _ -> raise (Invalid_argument ("ast_of_term: " ^ string_of_term e));
);;


(* `value` é um tipo extensível *)
type value = ..
;;


(* valores de L2*)
type value +=
  | VInt of int                 (* valor de um inteiro *)
  | VBool of bool               (* valor de um booleano *)
  | None                        (* "the value of Nothing is None" *)
  | MemoryPosition of int       (* l *)
  | Error of string
;;

(* repr. string de um valor *)
let rec string_of_value (v: value) : string = (match v with
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b
  | None -> "()"
  | MemoryPosition l -> "!" ^ string_of_int l
  | Error s -> "Error: " ^ s
  | _ -> failwith "string_of_value not implemented"
);;


(* um termo é um valor se puder ser avaliado imediatamente como valor *)
let is_value (e: term) : bool = match e with
  | Integer _ -> true
  | Boolean _ -> true
  | Location _ -> true
  | Nothing -> true
  | _ -> false
and value_of_term (e: term) : value = match e with
  | Integer n -> VInt n
  | Boolean b -> VBool b
  | Location l -> MemoryPosition l
  | Nothing -> None
  | RuntimeError s -> Error s
  | _ -> raise (Invalid_argument "value_of_term")
;;