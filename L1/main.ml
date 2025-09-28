open Terms
open Types

let _ =
  Results.print_terms ([

  (* let x = 1 in let y = 2 in if true then x else y *)
    VarDefinition((Integer 1, "x"),
      VarDefinition((Integer 2, "y"),
        Conditional(
          Fst(
            OrderedPair(
              Boolean true,
              Boolean false
            )
          ),
          Identifier "x",
          Identifier "y"
        )
      )
    );


    (* exceção fst e, typeof(e) <> Pair t,t' *)
    Fst(
      Integer 1
    );

    (* exceção snd e, typeof(e) <> Pair t,t' *)
    Snd(
      Boolean true
    );

    (* exceção if (e1, e2, e3), typeof(e1) <> Bool *)
    Conditional(
      Integer 1,
      Boolean true,
      Boolean false
    );

    (* exceção if (e1, e2, e3), typeof(e2) <> typeof(e3) *)
    Conditional(
      Boolean true,
      Integer 1,
      Boolean true
    );

    (* unbound variable exception *)
    Identifier "x";
  ])