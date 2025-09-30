(* dado um termo `e`, retorna um par contendo sua repr. string e sua AST (string) *)
let rec str (e: Terms.term) : (string * string) =
  let r : string = Terms.string_of_term e in
  let s : string = Terms.ast_of_term e in
  (r, s)
;;


(*  ambiente de tipos: associa a cada NameBinding um tipo *)
type env = (Terms.binding * Types.tipo) list;;


(*  repr. string de um ambiente de tipos Γ 
  "[(x, e, t) :: (y, e, t) :: Γ]" etc
*)
let string_of_env (env: env) : string = (
  let rec aux (env: env) : string = (
    match env with
    | [] -> "Γ"
    | ((e, y), t)::tl -> "(" ^ y ^ ", " ^ Terms.string_of_term e ^ ", " ^ Types.string_of_tipo t ^ ") :: " ^ aux tl
  )
  in (
    "[" ^ aux env ^ "]"
  )
);;