open Ast

type t [@@deriving sexp]

val of_regex : regex -> t
val to_dot : t -> string
