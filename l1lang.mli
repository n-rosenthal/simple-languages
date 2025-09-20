exception ProgrammingError of string
exception TypeError of string
exception RuntimeError of string
type tipo =
    Boolean
  | Integer
  | OrderedPair of tipo * tipo
  | Arrow of tipo * tipo
  | RecursiveFn of tipo
val string_of_tipo : tipo -> string
val type_scheme_rules : (string * string) list
val replace : string -> string -> string -> string
val replace_list : string -> string list -> string list -> string
val get_typerule : string -> string list -> (string, exn) result
