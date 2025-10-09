(**
  Sistema de tipos para a linguagem L2

  version 1.0
*)

(*  tipos de L2 *)
type tipo = ..
;;

type tipo +=
  | Int                     (* inteiros *)
  | Bool                    (* booleanos *)
  | Ref of tipo             (* referência *)
  | Unit                    (* (), tipo unit/VoidType *)
;;


(* repr. string de um tipo *)
let rec string_of_tipo (t: tipo) : string = match t with
  | Int -> "int"
  | Bool -> "bool"
  | Ref t -> "ref " ^ string_of_tipo t
  | Unit -> "unit"
  | _ -> failwith "string_of_tipo"
;;
