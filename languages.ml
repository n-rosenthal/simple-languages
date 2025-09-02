(*
  Implementation of the `arith` language (TAPL chapter 3) in OCaml.
  The `arith` language is an extension over some numeric operations for the untyped lambda calculus.
  Consider the S_i family of languages, where 
    S_0 = {}
    S_1 = {true, false, 0} | {succ t, pred t, iszero t} | S_{if t_1 then t_2 else t_3}
    S_2 = S_1 | {pair t_1 t_2, fst t, snd t}
    S_3 = S_2 | {let x = t_1 in t_2}
    S_4 = S_3 | {fun x -> t}
    S_5 = S_4 | {apply t_1 t_2}
    S_6 = S_5 | {fix f -> t}
    S_7 = S_6 | {letrec f = t in t}

  The `arith` language is defined over S_1 and doesn't have a type system.

  This module DEFINES the LANGUAGES
  - `s1`   :          simple language, implicitly typed over {Boolean, Natural}
    - `arith`:        a language of terms for simple arithmetic operations, untyped, over S_1.
    - `arith_ ic` :   a intermediate representation of instruction codes that allows compilation from `arith`
*)

(*  Erros mais Interessantes  *)
(* TypeError of
  -  nome do erro
  -  código do erro
  - mensagem do erro
  - informação adicional opcional
*)
exception TypeError of string * string * string * string option;;

(* RuntimeError of
  -  nome do erro
  -  código do erro
  - mensagem do erro
  - informação adicional opcional
*)
exception RuntimeError of string * int * string * (string list) option;;



let string_of_exn (e: exn) : string = match e with
  | TypeError (name, code, message, info) -> "TypeError: " ^ name ^ " (" ^ code ^ "): " ^ message ^ "\n" ^ (match info with None -> "" | Some s -> s ^ "\n")
  | RuntimeError (name, code, message, info) ->
      "RuntimeError: " ^ name ^ " (" ^ string_of_int code ^ "): " ^ message ^ "\n" ^
      (match info with
        | None -> ""
        | Some s -> (String.concat "\n" s) ^ "\n"
      );
  | _ -> raise e
;;

(*
  ---
  `S_1`
  syntax of terms; values of s1;
  ---
*)

(* terms of `s1` *)
type s1_term = 
| True                  (*  first constructor of `Boolean` *)
| False                 (*  second constructor of `Boolean` *)
| Zero                  (*  main constructor of `Natural` *)

| Succ of s1_term      (*  successor constructor of `Natural` *)
| Pred of s1_term      (*  predecessor constructor of `Natural` *)

| IsZero of s1_term    (*  unary operator over `Natural`. *)

(*  control structures *)
| If of s1_term * s1_term * s1_term  (*  if (e_1 : Boolean) then (e_2 : T) else (e_3 : T) *)
;;

(*  implicit type system for `s1` *)
type s1_type =
  | Boolean         (*  True, False  *)
  | Natural         (*  Zero         *)
;;

(*  returns true if `e` is a value in `s1` *)
let is_value (e: s1_term) : bool = (match e with 
  | True | False | Zero -> true
  | _                   -> false
);;

(* returns true if `e` can be reduced to a numeric value *)
let rec is_numerical (e: s1_term) : bool = (match e with
  | Zero        -> true
  | Succ e'     -> is_numerical e'
  | Pred e'     -> is_numerical e'
  | _           -> false
);;


(*  returns the string representation of a term  *)
let rec string_of_term (e: s1_term) : string = (match e with
  | True  -> "true"
  | False -> "false"
  | Zero  -> "0"
  | Succ e' -> "succ(" ^ string_of_term e' ^ ")"
  | Pred e' -> "pred(" ^ string_of_term e' ^ ")"
  | IsZero e' -> "iszero(" ^ string_of_term e' ^ ")"
  | If (e1, e2, e3) -> "if " ^ string_of_term e1 ^ " then " ^ string_of_term e2 ^ " else " ^ string_of_term e3
);;

(*  returns the string representation of a type  *)
let string_of_type (t: s1_type) : string = (match t with
  | Boolean -> "bool"
  | Natural -> "nat"
);;

(* returns the integer representation of a numeric S_1 abstract synt. tree
  
  @raise  TypeError if `e` isn't numerical 
*)
let rec int_of_term (e: s1_term) : (int, exn) result = (match e with
  | Zero    -> Ok 0
  | Succ e' -> (match int_of_term e' with
    | Ok n    -> Ok (n + 1)
    | Error e -> Error e
  );
  | Pred e' -> (match int_of_term e' with
    | Ok n    -> Ok (n - 1)
    | Error e -> Error e
  );
  | _       -> Error (TypeError ("[IntOfTerm NotNumerical]", "400", "not numerical", Some (string_of_term e)))
);;

(* returns the natural number representation of a term (if it exists) *)
let nat_of_term (e: s1_term) : (int, exn) result = (match int_of_term e with
  | Ok n when n >= 0  -> Ok n;
  | _                 -> Error (TypeError ("[NatOfTerm NotNatural]", "401", "not natural", Some (string_of_term e)))
);;

(*  string representation of the term as a value, replacing numeric values with their integer representation  *)
let rec string_of_value (e: s1_term) : string = (match e with
  | True  -> "true"
  | False -> "false"
  | Zero  -> "0"
  | Succ e' -> (match int_of_term e' with
    | Ok n -> string_of_int (n + 1)
    | Error e -> raise e
  );
  | Pred e' -> (match int_of_term e' with
    | Ok n -> string_of_int (n - 1)
    | Error e -> raise e
  );
  | IsZero e' -> (match int_of_term e' with
    | Ok n -> string_of_int n
    | Error e -> raise e
  );
  | If (e1, e2, e3) -> "if " ^ string_of_value e1 ^ " then " ^ string_of_value e2 ^ " else " ^ string_of_value e3
);;

(*
  ---
  type inference for `s1` 
  ---
*)

let rec typeinfer (e: s1_term) : (s1_type, exn) result =
  (match e with
    | True  | False                       ->  Ok Boolean;
    | Zero                                ->  Ok Natural;
    | Succ e' | Pred e' | IsZero e'       -> typeinfer e';
    | If (e1, e2, e3) ->
        if (typeinfer e1) = Ok Boolean then
          (match typeinfer e2, typeinfer e3 with
            | Ok t, Ok t' when t = t'     ->  Ok t;
            | Ok t, Ok t' -> (
                Error (TypeError ("[IfTypeMismatch]", "402", " If (e1, e2, e3) expects that `e2` and `e3` have the same type, got `" ^ string_of_type t ^ "` and `" ^ string_of_type t' ^ "`", Some (string_of_term e)))
            )
            | _ -> (
              Error (TypeError ("[IfTypeMismatch]", "403", " If (e1, e2, e3) expects that `e2` and `e3` have the same type, got `None`", Some (string_of_term e)))
            )
          )
        else (
          Error (TypeError ("[IfTypeMismatch]", "404", " If (e1, e2, e3) expects that `e1` has type `Boolean`, got `None`", Some (string_of_term e)))
        )
    | _                                     ->  Error (TypeError ("[TypeInference]", "405", " type inference failed", Some (string_of_term e)))
  );;


(*
  ---
  evaluation for `s1`
  ---
*)

(*  does one step in the evaluation of term `e` if possible *)
let step (e: s1_term) : s1_term option = (match e with
  | If (True, e2, e3) -> Some e2
  | If (False, e2, e3) -> Some e3
  | _ -> None
);;

(*  evaluates a term `e` until it can't be reduced anymore  *)
let rec eval (e: s1_term) : s1_term = (match step e with
  | Some e' -> eval e'
  | None    -> e
);;



(*
  ---
  interpreter for `s1` language
  ---
*)
let interpreter (e: s1_term) : (s1_term, exn) result =(
  match typeinfer e with
  | Error e ->  (
    print_endline "type inference failed";
    Error e
  );

  | Ok t -> (
    let e' = eval e in
    match typeinfer e' with
    | Error e ->  (
      print_endline "type inference failed";
      Error e
    );
    | Ok t -> (
      print_endline ("input: `" ^ string_of_term e ^ "`");
      print_endline ("output: `" ^ string_of_term e' ^ "` (as_value=) `" ^ string_of_value e' ^ "`");
      Ok e'
    )
  );
);;


(*  main  *)
let () =
  match (interpreter (If (IsZero (Succ (Pred (Zero))),
                          True,
                          False))) with
  | Ok e -> print_endline ("interpreter was successful");
  | Error e -> print_endline ("interpreter failed: " ^ (string_of_exn e));
;;