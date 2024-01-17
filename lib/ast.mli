(* Anchors *)
type anchor =
  | AWordBoundary
  | ANonWordBoundary
  (* | AStartOfStringOnly *)
  (* | AEndOfStringOnlyNotNewline *)
  (* | AEndOfStringOnly *)
  (* | APreviousMatchEnd *)
  | AEndOfString
  | AStartOfString
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
[@@deriving sexp]

(* CharacterClassFromUnicodeCategory ::= "\p{" UnicodeCategoryName "}" *)
(* UnicodeCategoryName ::= Letters *)

type char_group_itm =
  | CClass of char_class
  (* | CharacterClassFromUnicodeCategory *)
  | CRange of char * char
  | Char of char
[@@deriving sexp]

type char_group = bool * char_group_itm list [@@deriving sexp]

(* Match Types *)
type match_char_class = CharGroup | CharClass | CharClassFromUnicodeCategory
[@@deriving sexp]

type match_itm =
  | MatchAnyChar
  | MatchCharClass of match_char_class
  | MatchChar of char
[@@deriving sexp]

type match_typ = match_itm * quantifier option [@@deriving sexp]

(* Expression & Group Types *)
type expression = subexpression list [@@deriving sexp]

and subexpression = Match | Group of group_typ | Anchor | Backreference
[@@deriving sexp]

and group_typ = bool * expression * quantifier option [@@deriving sexp]

type regex_typ = bool * expression [@@deriving sexp]
