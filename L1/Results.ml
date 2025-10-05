(* Pretty-printing of results *)
open Printf

let n_string (ss: string list) (n: int) : string =
  try
    List.nth ss n
  with
  | Failure _ -> ""
  | _ -> ""
;;

let rec repeat (s: string) (n: int) : string =
  if n <= 0 then
    ""
  else
    s ^ repeat s (n-1)
;;

let length (s: string) : int =
  String.length s
;;

let split_at (n: int) (s: string) : (string * string) option =
  if (length s) <= n then
    Some (s, "")
  else
    Some (String.sub s 0 n, String.sub s n (length s - n))
;;

let break (s: string) (n: int) : string list =
  let rec aux (s: string) (n: int) (acc: string list) : string list =
    if (length s) <= n then
      List.rev (s :: acc)
    else
      (match split_at n s with
        | Some (s1, s2) ->  aux s2 n (s1 :: acc)
        | None -> List.rev acc
      )
  in
  aux s n []
;;

(* helper for indentation *)
let indent_string (n:int) : string =
  String.make (2*n) ' '

(* if `s` is shorten than `size`, returns `s` with whitespace appended to the end;
   if `s` is longer than `size`, returns the first `size` characters of `s` followed by '...' *)
let process_string (s: string) (size: int) : string =
  if (length s) <= size then
    s ^ repeat " " (size - (length s))
  else
    String.sub s 0 (size-3) ^ "..."
  ;;


(* Pretty-print "expression-like" style *)
let rec printlist_of_term ?(indent=0) (t: Terms.term) : string list =
  match t with
  | None -> [indent_string indent ^ "()"]
  | Integer n -> [indent_string indent ^ string_of_int n]
  | Boolean b -> [indent_string indent ^ string_of_bool b]
  | OrderedPair (a,b) ->
      [indent_string indent ^ "("]
      @ printlist_of_term ~indent:(indent+1) a
      @ printlist_of_term ~indent:(indent+1) b
      @ [indent_string indent ^ ")"]
  | Fst e ->
      [indent_string indent ^ "fst"]
      @ printlist_of_term ~indent:(indent+1) e
  | Snd e ->
      [indent_string indent ^ "snd"]
      @ printlist_of_term ~indent:(indent+1) e
  | Conditional (e1,e2,e3) ->
      [indent_string indent ^ "if"]
      @ printlist_of_term ~indent:(indent+1) e1
      @ [indent_string indent ^ "then"]
      @ printlist_of_term ~indent:(indent+1) e2
      @ [indent_string indent ^ "else"]
      @ printlist_of_term ~indent:(indent+1) e3
  | _ -> [indent_string indent ^ "Unknown"]

(* Pretty-print "AST-like" style *)
let rec printlist_of_ast ?(indent=0) (t:Terms.term) : string list =
  match t with
  | None -> [indent_string indent ^ "None"]
  | Integer n -> [indent_string indent ^ sprintf "Integer(%d)" n]
  | Boolean b -> [indent_string indent ^ sprintf "Boolean(%b)" b]
  | OrderedPair (a,b) ->
      [indent_string indent ^ "OrderedPair("]
      @ printlist_of_ast ~indent:(indent+1) a
      @ printlist_of_ast ~indent:(indent+1) b
      @ [indent_string indent ^ ")"]
  | Fst e ->
      [indent_string indent ^ "Fst("]
      @ printlist_of_ast ~indent:(indent+1) e
      @ [indent_string indent ^ ")"]
  | Snd e ->
      [indent_string indent ^ "Snd("]
      @ printlist_of_ast ~indent:(indent+1) e
      @ [indent_string indent ^ ")"]
  | Conditional (e1,e2,e3) ->
      [indent_string indent ^ "Conditional("]
      @ printlist_of_ast ~indent:(indent+1) e1
      @ printlist_of_ast ~indent:(indent+1) e2
      @ printlist_of_ast ~indent:(indent+1) e3
      @ [indent_string indent ^ ")"]
  | _ -> [indent_string indent ^ "Unknown"]
  let print_term (box_size : int) (e: Terms.term) : unit =
    let side_size = box_size / 2 in
  
    let terms_lines = printlist_of_term e in
    let ast_lines   = printlist_of_ast e in
    let n_lines     = max (List.length terms_lines) (List.length ast_lines) in
  
    let get_line lst i =
      if i < List.length lst then List.nth lst i else ""
    in
  
    (* ========== Expression vs AST ========== *)
    print_endline ("╔" ^ repeat "═" side_size ^ "═╦═" ^ repeat "═" side_size ^ "╗");
    print_endline ("║ expression" ^ repeat " " (side_size - 10) ^
                   "║ abstract syntax tree" ^ repeat " " (side_size - 20) ^ "║");
    print_endline ("╠" ^ repeat "═" side_size ^ "═╬═" ^ repeat "═" side_size ^ "╣");
  
    for i = 0 to n_lines - 1 do
      let curr_term = get_line terms_lines i in
      let curr_ast  = get_line ast_lines i in
      let term_repr = process_string curr_term side_size in
      let ast_repr  = process_string curr_ast side_size in
      print_endline ("║ " ^ term_repr ^ "║ " ^ ast_repr ^ "║");
    done;
  
    (* ========== Type inference ========== *)
    print_endline ("╠" ^ repeat "═" side_size ^ "═╩═" ^ repeat "═" side_size ^ "╣");
    print_endline ("║ static type inference" ^
                   repeat " " (2 * side_size - 19) ^ "║");
    print_endline ("╠" ^ repeat "═" (2 * side_size + 2) ^ "═╣");
  
    let t, env, rules = Typeinfer.typeinfer e [] in
    let t_repr = process_string (Types.string_of_tipo t) (box_size - 5) in
    print_endline ("║  type: " ^ t_repr ^ "║");
    print_endline ("╠" ^ repeat "═" (2 * side_size + 2) ^ "═╣");
  
    List.iteri (fun i (name, application) ->
      let index_repr = Printf.sprintf "(%02d.)" (i+1) in
      let lhs_size = (3 * side_size - 3) / 5 in
      let rhs_size = side_size + side_size / 4 in
      let lhs = process_string (index_repr ^ " " ^ name) lhs_size in
      let rhs = process_string application rhs_size in
      print_endline ("║  " ^ lhs ^
                     repeat " " (3 + lhs_size - String.length lhs) ^
                     rhs ^
                     repeat " " (3 + rhs_size - String.length rhs) ^
                     "║")
    ) rules;
  
    print_endline ("╚" ^ repeat "═" (2 * side_size + 2) ^ "╝");
  
    (* Debug info, optional *)
    Printf.printf "ambiente de tipos: %s\n\n" (Middleware.string_of_env env);
  
    (* ========== Evaluation ========== *)
    try
      let v, rules = Eval.eval e [] [] in
      (* Header box *)
      print_endline ("╔" ^ repeat "═" (2 * side_size + 2) ^ "╗");
      print_endline ("║ evaluation" ^
                     repeat " " (2 * side_size - 8) ^ "║");
      print_endline ("╠" ^ repeat "═" (2 * side_size + 2) ^ "╣");
      (* Value *)
      let v_repr = process_string (Terms.string_of_value v) (box_size - 5) in
      print_endline ("║  value: " ^ v_repr ^ "║");
      print_endline ("╠" ^ repeat "═" (2 * side_size + 2) ^ "╣");
      (* Rules *)
      List.iteri (fun i (name, application) ->
        let index_repr = Printf.sprintf "(%02d.)" (i+1) in
        let lhs_size = (3 * side_size - 3) / 5 in
        let rhs_size = side_size + side_size / 4 in
        let lhs = process_string (index_repr ^ " " ^ name) lhs_size in
        let rhs = process_string application rhs_size in
        print_endline ("║  " ^ lhs ^
                       repeat " " (3 + lhs_size - String.length lhs) ^
                       rhs ^
                       repeat " " (3 + rhs_size - String.length rhs) ^
                       "║")
      ) rules;
      print_endline ("╚" ^ repeat "═" (2 * side_size + 2) ^ "╝")
    with
    | Eval.RuntimeError s ->
        print_endline ("╔" ^ repeat "═" (2 * side_size + 2) ^ "╗");
        print_endline ("║ evaluation failed" ^
                       repeat " " (2 * side_size - 16) ^ "║");
        print_endline ("╠" ^ repeat "═" (2 * side_size + 2) ^ "╣");
        let s_repr = process_string s (box_size - 5) in
        print_endline ("║  error: " ^ s_repr ^ "║");
        print_endline ("╚" ^ repeat "═" (2 * side_size + 2) ^ "╝")
  ;;
  
let print_terms (es: Terms.term list) : unit =
  List.iter (print_term 129) es
;;
