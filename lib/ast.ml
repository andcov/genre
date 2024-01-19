open! Base

(* Anchors *)
type anchor = AWordBoundary | ANonWordBoundary | AEndOfString | AStartOfString
[@@deriving sexp]

(* Quantifier Types *)
type range_typ = Exactly of int | Least of int | Range of int * int
[@@deriving sexp]

type quantifier_typ =
  | QZeroOrMore
  | QOneOrMore
  | QZeroOrOne
  | QRange of range_typ
[@@deriving sexp]

type quantifier = quantifier_typ * bool [@@deriving sexp]

(* Character Classes *)
type char_class =
  | CClassAnyWord
  | CClassAnyWordInv
  | CClassAnyDigit
  | CClassAnyDigitInv
  | CClassWhitespace
  | CClassWhitespaceInv
[@@deriving sexp]

type char_group_itm =
  | CClass of char_class
  | CRange of char * char
  | CChar of char
[@@deriving sexp]

type char_group = bool * char_group_itm list [@@deriving sexp]

(* Match Types *)
type match_itm =
  | MAnyChar
  | MCharClass of char_class
  | MCharGroup of char_group (* | MCharClassFromUnicodeCategory *)
  | MChar of char
  | MStr of string
[@@deriving sexp]

type match_typ = match_itm * quantifier option [@@deriving sexp]

(* Expression & Group Types *)
type expression =
  | Node of subexpression list * expression
  | Leaf of subexpression list
[@@deriving sexp]

and subexpression =
  | Group of group
  | Anchor of anchor
  | Backreference of int
  | Match of match_typ
[@@deriving sexp]

and group = bool * expression * quantifier option [@@deriving sexp]

type regex = bool * expression [@@deriving sexp]

let rec optimize_chars_exp = function
  | Node (se_l, e) -> Node (optimize_chars_subexp se_l, optimize_chars_exp e)
  | Leaf se_l -> Leaf (optimize_chars_subexp se_l)

and optimize_chars_subexp (sub_exps : subexpression list) : subexpression list =
  match sub_exps with
  | Match (MChar c1, None) :: Match (MChar c2, None) :: rst ->
      let s = Char.(to_string c1 ^ to_string c2) in
      Match (MStr s, None) :: rst |> optimize_chars_subexp
  | Match (MStr s, None) :: Match (MChar c, None) :: rst ->
      let s' = s ^ Char.to_string c in
      Match (MStr s', None) :: rst |> optimize_chars_subexp
  | Group (non_cap, e, q) :: rst ->
      Group (non_cap, optimize_chars_exp e, q) :: optimize_chars_subexp rst
  | s :: rst -> s :: optimize_chars_subexp rst
  | [] -> []

let optimize (rg : regex) : regex =
  let a, e = rg in
  (a, optimize_chars_exp e)
