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
  | Match of match_typ (* | Subexpression of subexpression *)
[@@deriving sexp]

and group = bool * expression * quantifier option [@@deriving sexp]

type regex = bool * expression [@@deriving sexp]
