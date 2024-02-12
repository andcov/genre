open Ast

type t [@@deriving sexp]

val of_regex : regex -> t
val to_dot : t -> string
val match_string : t -> string -> (int, string) result
