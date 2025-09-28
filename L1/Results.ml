(* Pretty-printing of results *)

let print_term (e: Terms.term) : unit =
  (* Expression and AST *)
  print_endline "--- detalhes da expressão ---";
  Printf.printf "expr: %s\nAST: %s\n\n"
    (Terms.string_of_term e)
    (Terms.string_of_ast e);

  (* size, depth, constants *)
  let size  = Terms.size e in
  let depth = Terms.depth e in
  let consts = Terms.constants e |> String.concat ", " in
  Printf.printf "size: %d\ndepth: %d\nconstants: [%s]\n\n"
    size depth consts;

  (* Type inference *)
  (let t, env, rules = Types.typeinfer e [] in
      Printf.printf "tipo:\n (%s) : %s\n\n"
        (Terms.string_of_term e)
        (Types.string_of_tipo t);

      (* ambiente de tipos *)
      Printf.printf "ambiente de tipos: %s\n\n" (Types.string_of_env env);

      (** esquemas de regras de inferência substituídos pelos {termos, valores, tipos} 
      List.iteri (fun index (r, t) ->
        let rule' = Repr.substitute (Repr.get_rule r) t in
        Printf.printf "(%3d) {%15s} %s\n" (index+1) r rule') rules
      ;
      *)

      print_endline "";
    );

  (* Evaluation *)
  let v, env = Eval.eval e [] [] in
  Printf.printf "avaliação: %s\n\n" (Terms.string_of_value v);

  (* ambiente de valores *)
  Printf.printf "ambiente de valores: %s\n\n" (Eval.string_of_env env)
;;

let print_terms (es: Terms.term list) : unit =
  List.iter print_term es
;;
