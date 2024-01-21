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

(* Backreference Types *)
type backreference = { q : quantifier option; id : int } [@@deriving sexp]

(* Expression & Group Types *)
type group_typ = Capturing of int | Noncapturing [@@deriving sexp]

type regex = expression list [@@deriving sexp]
and expression = subexpression list [@@deriving sexp]

and subexpression =
  | Group of group
  | Anchor of anchor
  | Backreference of backreference
  | Match of match_typ
[@@deriving sexp]

and group = { cap : group_typ; inner : regex; q : quantifier option }
[@@deriving sexp]
