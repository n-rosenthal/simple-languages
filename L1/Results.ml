(* Pretty-printing of results *)

let print_term (e: Terms.term) : unit =
  (* Expression and AST *)
  print_endline "--- detalhes da expressão ---";
  Printf.printf "expr: %s\nAST: %s\n\n"
    (Terms.string_of_term e)
    (Terms.ast_of_term e);

  (* size, depth, constants *)
  let size  = Terms.size e in
  let depth = Terms.depth e in
  let consts = Terms.constants e |> String.concat ", " in
  Printf.printf "size: %d\ndepth: %d\nconstants: [%s]\n\n"
    size depth consts;

  (* Type inference *)
  (let t, env, rules = Typeinfer.typeinfer e [] in
      Printf.printf "tipo:\n (%s) : %s\n\n"
        (Terms.string_of_term e)
        (Types.string_of_tipo t);

      (* ambiente de tipos *)
      Printf.printf "ambiente de tipos: %s\n\n" (Middleware.string_of_env env);
      (* regras de inferência de tipos *)
      print_endline "--- regras de inferência de tipos ---";
      for i = 0 to List.length rules - 1 do
        let (name, application) = List.nth rules i in
        Printf.printf "(%-4d.) (%36s) \t%s\n" i name application
      done;
  );

  (** Evaluation 
  let v, env = Eval.eval e [] [] in
  Printf.printf "avaliação: %s\n\n" (Terms.string_of_value v);

   ambiente de valores 
  Printf.printf "ambiente de valores: %s\n\n" (Eval.string_of_env env)
  *)
;;

let print_terms (es: Terms.term list) : unit =
  List.iter print_term es
;;
