(*
  Pretty-printing de resultados
*)

let print_term (e: Terms.term) : unit =
  (* `e` *)
  print_endline ("expr: " ^ Terms.string_of_term e ^ "\n" ^ Terms.string_of_ast e);

  (* `size(t)`, `depth(t)`, `constants(t)` *)
  print_endline ("\t(size: " ^ (string_of_int (Terms.size e)));
  print_endline ("\t(depth: " ^ (string_of_int (Terms.depth e)));
  print_endline ("\t(constants: " ^ (String.concat ", " (Terms.constants e)) ^ ")");
  print_endline "";

  match (Types.typeinfer e) with
  | Ok (t, rules) -> (
    (* `t` *)
    print_endline ("(" ^ (Terms.string_of_term e) ^ " : " ^ (Types.string_of_tipo t) ^ ")");

    (* `rules` *)
    print_endline "";
    List.iter
      (fun (rule, terms) ->
        Printf.printf "%s\n\t%s\n" rule (String.concat "\n\t" terms))
          rules;
        print_endline ""
  )
  | Error e -> (
    print_endline ("Error: " ^ (Types.string_of_exn e));
  )
;;

let print_terms (es: Terms.term list) : unit =
  List.iter print_term es
;;