open Terms
open Types
open Typeinfer

let _ =
  Results.print_terms ([
    (* Identifier "x" *)
    VarDefinition("x", Integer 1, (Fst (OrderedPair(Identifier "x", Identifier "x"))));
  ])