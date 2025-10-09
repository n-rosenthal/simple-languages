(**
  "Interpretador" para a linguagem L2
  
  version 1.0
*)

open Types
open Terms
open TypeInference
open Evaluation

(* dada uma lista de termos, infere seus tipos e os avalia *)
let interpret (terms: term list) : unit = (
  List.iter (fun e -> (
    let t, env, rules = typeinfer e [] in print_rules rules;
    let v, ctx, rules = eval e [] in print_rules rules;
  )) terms
  )
;;

let _ = interpret [
  VarDefinition ("x", Conditional (Boolean true, Integer 1, Integer 0), Int,
    VarDefinition ("y", Conditional (Boolean false, Integer 0, Integer 1), Int,
      Conditional (Boolean true, Identifier "x", Identifier "y")
    )
  )
]