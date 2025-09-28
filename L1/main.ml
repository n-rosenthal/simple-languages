open Terms
open Types

let _ =
  Results.print_terms ([
    Fst(
      OrderedPair(
        OrderedPair(
          Boolean true,
          Boolean false
        ),
        OrderedPair(
          Boolean false,
          Boolean true
        )
      )
    );

    Snd(
      OrderedPair(
        OrderedPair(
          Integer 1,
          Integer 2
        ),
        OrderedPair(
          Integer 3,
          Integer 4
        )
      )
    )
  ])