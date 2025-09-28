let terms : Terms.term list = [
  (*  Integer (-1) *)
  (Terms.Integer (-1));

  (*  Integer 0 *)
  (Terms.Integer 0);

  (*  Integer 1 *)
  (Terms.Integer 1);

  (*  Boolean true *)
  (Terms.Boolean true);

  (*  Boolean false *)
  (Terms.Boolean false);

  (*  OrderedPair (Integer 0, Integer 1) *)
  (Terms.OrderedPair (Terms.Integer 0, Terms.Integer 1));

  (*  OrderedPair (Integer (-1), Boolean true) *)
  (Terms.OrderedPair (Terms.Integer (-1), Terms.Boolean true));

  (* fst (true, false) *)
  (Terms.Fst (Terms.OrderedPair (Terms.Boolean true, Terms.Boolean false)));

  (* snd (false (0, true))*)
  (Terms.Snd (Terms.OrderedPair (Terms.Boolean false, Terms.OrderedPair (Terms.Integer 0, Terms.Boolean true))));
]

in

Results.print_terms terms
;;