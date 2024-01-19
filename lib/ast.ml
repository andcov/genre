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

type quantifier = { greedy : bool; typ : quantifier_typ } [@@deriving sexp]

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

type char_group = { neg : bool; inner : char_group_itm list } [@@deriving sexp]

(* Match Types *)
type match_itm =
  | MAnyChar
  | MCharClass of char_class
  | MCharGroup of char_group (* | MCharClassFromUnicodeCategory *)
  | MChar of char
  | MStr of string
[@@deriving sexp]

type match_typ = { q : quantifier option; itm : match_itm } [@@deriving sexp]

(* Expression & Group Types *)
type group_typ = Capturing of int | Noncapturing [@@deriving sexp]

type regex = expression list [@@deriving sexp]
and expression = subexpression list [@@deriving sexp]

and subexpression =
  | Group of group
  | Anchor of anchor
  | Backreference of int
  | Match of match_typ
[@@deriving sexp]

and group = { cap : group_typ; inner : regex; q : quantifier option }
[@@deriving sexp]

let rec optimize_chars (rg : regex) = List.map rg ~f:optimize_chars_exp

and optimize_chars_exp (exp : expression) : expression =
  match exp with
  | Match { itm = MChar c1; q = None }
    :: Match { itm = MChar c2; q = None }
    :: rst ->
      let itm = MStr Char.(to_string c1 ^ to_string c2) in
      Match { itm; q = None } :: rst |> optimize_chars_exp
  | Match { itm = MStr s; q = None } :: Match { itm = MChar c; q = None } :: rst
    ->
      let itm = MStr (s ^ Char.to_string c) in
      Match { itm; q = None } :: rst |> optimize_chars_exp
  | Group g :: rst ->
      Group { g with inner = optimize_chars g.inner } :: optimize_chars_exp rst
  | s :: rst -> s :: optimize_chars_exp rst
  | [] -> []
