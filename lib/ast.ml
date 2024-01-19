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

let rec id_groups_rg (rg : regex) (id : int) : regex * int =
  List.fold_left rg ~init:([], id) ~f:(fun (acc, id) exp ->
      let exp', id' = id_groups_exp exp id in
      (exp' :: acc, id'))
  |> fun (rg', id') -> (List.rev rg', id')

and id_groups_exp (exp : expression) (id : int) : expression * int =
  let add_capturing_group cap id =
    match cap with
    | Capturing _ -> (Capturing id, id + 1)
    | Noncapturing -> (Noncapturing, id)
  in
  match exp with
  | Group g :: rst ->
      let cap', id' = add_capturing_group g.cap id in
      let inner', id'' = id_groups_rg g.inner id' in
      let rst', id''' = id_groups_exp rst id'' in
      (Group { g with cap = cap'; inner = inner' } :: rst', id''')
  | s :: rst ->
      let rst', id' = id_groups_exp rst id in
      (s :: rst', id')
  | [] -> ([], id)

let optimize rg =
  let rg' = rg |> optimize_chars in
  id_groups_rg rg' 1 |> fst
