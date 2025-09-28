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
    )
  ])