open Terms
open Types

let _ =
  Results.print_terms ([
    (* if true then () else () *)
    Conditional (Boolean true, None, None)
  ])