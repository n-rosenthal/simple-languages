open Terms
open Types

let _ =
  Results.print_terms ([
    (* fst ( (true, false), (false, true) ) *)
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

    (* snd ( (1, 2), (3, 4) ) *)
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