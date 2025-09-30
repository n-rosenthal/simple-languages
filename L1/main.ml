open Terms
open Types
open Typeinfer

let _ =
  Results.print_terms ([
    (*  pares ordenados *)
    OrderedPair (Integer 1, Integer (-1));
    OrderedPair (Boolean true, Boolean false);
    OrderedPair (OrderedPair(Integer 1, Integer 2), OrderedPair(Integer 3, Integer 4));
    
    (* erro 1: (propagação de exn.) não é possível haver par (exn, _). qualquer (exn, _) -> exn. *)
    OrderedPair(Raise (Integer 1), Integer 2);

    (* erro 2: (propagação de exn.) não é possível haver par (_, exn). qualquer (_, exn) -> exn. *)
    OrderedPair(Integer 1, Raise (Integer 2));

    (* erro 3: (propagação de exn.) fst exn -> exn. *)
    Fst(OrderedPair(Raise (Integer 1), Integer 2));

    (* erro 4: (type error) fst e, e <> Pair. *)
    Fst(Integer 1);

    (* erro 5: (propagação de exn.) snd exn -> exn. *)
    Snd(OrderedPair(Integer 1, Raise (Integer 2)));

    (* erro 6: (type error) snd e, e <> Pair. *)
    Snd(Integer 1);

    (* ! propagação de exceções (tripla, máx. até então) *)
    Fst(OrderedPair(OrderedPair(Integer 1, Integer 2), OrderedPair(Integer 3, Snd (Integer 4))));
  
    (* erro 7: (propagação de exn.) if exn then e1 else e2 -> exn. *)
    Conditional (Raise (Integer 1), Integer 2, Integer 3);
  
    (* erro 8: (propagação de exn.) if e1 then exn else e2 -> exn. *)
    Conditional (Integer 1, Raise (Integer 2), Integer 3);
  
    (* erro 9: (type error) if e1 then e2 else e3, e1 <> e2. *)
    Conditional (Integer 1, Integer 2, Boolean true);

    (* erro 10: (type error) if e1 then e2 else e3, e1 <> Boolean. *)
    Conditional (Integer 1, Integer 2, Integer 3);
    
  ])