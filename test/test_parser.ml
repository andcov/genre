open! Base
open Genre.Ast
open Genre.Regex_parser
open Genre.Parser

let compare_quantifier q1 q2 =
  if Sexp.equal (sexp_of_quantifier q1) (sexp_of_quantifier q2) then 0 else 1

let compare_anchor a1 a2 =
  if Sexp.equal (sexp_of_anchor a1) (sexp_of_anchor a2) then 0 else 1

let compare_char_group c1 c2 =
  if Sexp.equal (sexp_of_char_group c1) (sexp_of_char_group c2) then 0 else 1

let%test_unit "test quantifier parsing" =
  let _ =
    [
      ("{34}", (QRange (Exactly 34), false));
      ("{1234,}", (QRange (Least 1234), false));
      ("{0,10}", (QRange (Range (0, 10)), false));
      ("{12}?", (QRange (Exactly 12), true));
      ("{93678,}?", (QRange (Least 93678), true));
      ("*", (QZeroOrMore, false));
      ("*?", (QZeroOrMore, true));
      ("+", (QOneOrMore, false));
      ("+?", (QOneOrMore, true));
      ("?", (QZeroOrOne, false));
      ("??", (QZeroOrOne, true));
    ]
    |> List.map ~f:(fun (str, res) ->
           [%test_eq: quantifier] (parse str p_quantifier) res)
  in
  ()

let%test_unit "test anchor parsing" =
  let _ =
    [
      ({|\b|}, AWordBoundary);
      ({|\B|}, ANonWordBoundary);
      ("^", AStartOfString);
      ("$", AEndOfString);
    ]
    |> List.map ~f:(fun (str, res) ->
           [%test_eq: anchor] (parse str p_anchor) res)
  in
  ()

let%test_unit "test character class parsing" =
  let _ =
    [
      ({|[\w]|}, (false, [ CClass CClassAnyWord ]));
      ({|[^\W]|}, (true, [ CClass CClassAnyWordInv ]));
      ({|[^\d]|}, (true, [ CClass CClassAnyDigit ]));
      ({|[\D]|}, (false, [ CClass CClassAnyDigitInv ]));
      ({|[^\D\w]|}, (true, [ CClass CClassAnyDigitInv; CClass CClassAnyWord ]));
      ({|[X]|}, (false, [ Char 'X' ]));
      ( {|[ab\w8]|},
        (false, [ Char 'a'; Char 'b'; CClass CClassAnyWord; Char '8' ]) );
      ({|[a-z]|}, (false, [ CRange ('a', 'z') ]));
      ( {|[^A-F\w ]|},
        (true, [ CRange ('A', 'F'); CClass CClassAnyWord; Char ' ' ]) );
    ]
    |> List.map ~f:(fun (str, res) ->
           [%test_eq: char_group] (parse str p_c_group) res)
  in
  ()
