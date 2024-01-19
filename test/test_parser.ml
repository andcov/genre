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

let compare_backreference b1 b2 =
  if Sexp.equal (sexp_of_backreference b1) (sexp_of_backreference b2) then 0
  else 1

let compare_subexpression s1 s2 =
  if Sexp.equal (sexp_of_subexpression s1) (sexp_of_subexpression s2) then 0
  else 1

let compare_match_typ m1 m2 =
  if Sexp.equal (sexp_of_match_typ m1) (sexp_of_match_typ m2) then 0 else 1

let%test_unit "test quantifier parsing" =
  let _ =
    [
      ("{34}", { typ = QRange (Exactly 34); greedy = true });
      ("{12}?", { typ = QRange (Exactly 12); greedy = false });
      ("{1234,}", { typ = QRange (Least 1234); greedy = true });
      ("{0,10}", { typ = QRange (Range (0, 10)); greedy = true });
      ("{93678,}?", { typ = QRange (Least 93678); greedy = false });
      ("*", { typ = QZeroOrMore; greedy = true });
      ("*?", { typ = QZeroOrMore; greedy = false });
      ("+", { typ = QOneOrMore; greedy = true });
      ("+?", { typ = QOneOrMore; greedy = false });
      ("?", { typ = QZeroOrOne; greedy = true });
      ("??", { typ = QZeroOrOne; greedy = false });
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

let%test_unit "test backreference parsing" =
  let _ =
    [
      ({|\1|}, { id = 1; q = None });
      ({|\16*?|}, { id = 16; q = Some { typ = QZeroOrMore; greedy = false } });
      ( {|\3{2,}|},
        { id = 3; q = Some { typ = QRange (Least 2); greedy = true } } );
    ]
    |> List.map ~f:(fun (str, res) ->
           [%test_eq: backreference] (parse str p_backreference) res)
  in
  ()

let%test_unit "test character class parsing" =
  let _ =
    [
      ({|[\w]|}, { neg = false; inner = [ CClass CClassAnyWord ] });
      ({|[^\W]|}, { neg = true; inner = [ CClass CClassAnyWordInv ] });
      ({|[^\d]|}, { neg = true; inner = [ CClass CClassAnyDigit ] });
      ({|[\D]|}, { neg = false; inner = [ CClass CClassAnyDigitInv ] });
      ( {|[^\D\w]|},
        {
          neg = true;
          inner = [ CClass CClassAnyDigitInv; CClass CClassAnyWord ];
        } );
      ({|[X]|}, { neg = false; inner = [ CChar 'X' ] });
      ( {|[ab\w8]|},
        {
          neg = false;
          inner = [ CChar 'a'; CChar 'b'; CClass CClassAnyWord; CChar '8' ];
        } );
      ({|[a-z]|}, { neg = false; inner = [ CRange ('a', 'z') ] });
      ( {|[^A-F\w ]|},
        {
          neg = true;
          inner = [ CRange ('A', 'F'); CClass CClassAnyWord; CChar ' ' ];
        } );
    ]
    |> List.map ~f:(fun (str, res) ->
           [%test_eq: char_group] (parse str p_c_group) res)
  in
  ()

let%test_unit "test match parsing" =
  let _ =
    [
      (".", { itm = MAnyChar; q = None });
      (".+", { itm = MAnyChar; q = Some { typ = QOneOrMore; greedy = true } });
      (".+?", { itm = MAnyChar; q = Some { typ = QOneOrMore; greedy = false } });
      ({|\w|}, { itm = MCharClass CClassAnyWord; q = None });
      ( {|\D{24}|},
        {
          itm = MCharClass CClassAnyDigitInv;
          q = Some { typ = QRange (Exactly 24); greedy = true };
        } );
      ( "[a-z]?",
        {
          itm = MCharGroup { neg = false; inner = [ CRange ('a', 'z') ] };
          q = Some { typ = QZeroOrOne; greedy = true };
        } );
      ("9", { itm = MChar '9'; q = None });
      ("Z*", { itm = MChar 'Z'; q = Some { typ = QZeroOrMore; greedy = true } });
    ]
    |> List.map ~f:(fun (str, res) ->
           [%test_eq: match_typ] (parse str p_match) res)
  in
  ()
