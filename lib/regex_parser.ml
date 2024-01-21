open! Base
open Ast
open Parser

let ( >> ) f g x = g (f x)

(* ------ Character Class parsers ------ *)

let p_generic_c_class (cls : string) (typ : char_class) =
  p_str ("\\" ^ cls) |> p_map (fun _ -> typ)

let p_c_class : char_class parser =
  p_any
    [
      p_generic_c_class "w" CClassAnyWord;
      p_generic_c_class "W" CClassAnyWordInv;
      p_generic_c_class "d" CClassAnyDigit;
      p_generic_c_class "D" CClassAnyDigitInv;
      p_generic_c_class "s" CClassWhitespace;
      p_generic_c_class "S" CClassWhitespaceInv;
    ]

let must_escape_in_char_class = String.mem {|^-]\|}

let p_c_char =
  p_any
    [
      p_character '\\' *> p_char |> p_filter must_escape_in_char_class;
      p_char |> p_filter (must_escape_in_char_class >> not);
    ]

(* TODO: validate range *)
let p_c_range : char_group_itm parser =
  p_c_char <* p_character '-' <*> p_c_char
  |> p_map (fun (l, u) -> CRange (l, u))

let p_c_group_itm : char_group_itm parser =
  p_any
    [
      p_c_class |> p_map (fun c_cls -> CClass c_cls);
      p_c_range;
      p_c_char |> p_map (fun ch -> CChar ch);
    ]

let p_c_group : char_group parser =
  p_str "[" *> ??(p_str "^")
  <*> p_one_or_more p_c_group_itm
  <* p_str "]"
  |> p_map (fun (neg_opt, inner) -> { neg = Option.is_some neg_opt; inner })

(* ------ Quantifier parsers ------ *)

let p_q_range : quantifier parser =
  p_str "{" *> p_number
  <*> ??(p_str "," *> ??p_number)
  <* p_str "}"
  <*> ??(p_str "?")
  |> p_map (fun ((l, op), lzy) ->
         let q =
           match op with
           | None -> Exactly l
           | Some None -> Least l
           | Some (Some u) -> Range (l, u)
         in
         { greedy = Option.is_none lzy; typ = QRange q })

let p_q_one_char (ch : char) (typ : quantifier_typ) =
  p_character ch *> ??(p_character '?')
  |> p_map (fun lzy -> { greedy = Option.is_none lzy; typ })

let p_q_zro_or_mor : quantifier parser = p_q_one_char '*' QZeroOrMore
let p_q_one_or_mor : quantifier parser = p_q_one_char '+' QOneOrMore
let p_q_zro_or_one : quantifier parser = p_q_one_char '?' QZeroOrOne

let p_quantifier : quantifier parser =
  p_any [ p_q_range; p_q_zro_or_mor; p_q_one_or_mor; p_q_zro_or_one ]

(* ------ Anchor parsers ------ *)

let p_a_word_bnd : anchor parser =
  p_str {|\b|} |> p_map (fun _ -> AWordBoundary)

let p_a_non_word_bnd : anchor parser =
  p_str {|\B|} |> p_map (fun _ -> ANonWordBoundary)

let p_a_start_of_str : anchor parser =
  p_str "^" |> p_map (fun _ -> AStartOfString)

let p_a_end_of_str : anchor parser = p_str "$" |> p_map (fun _ -> AEndOfString)

let p_anchor : anchor parser =
  p_any [ p_a_word_bnd; p_a_non_word_bnd; p_a_start_of_str; p_a_end_of_str ]

(* ------ Backreference parser ------ *)

let p_backreference : backreference parser =
  p_str {|\|} *> p_number <*> ??p_quantifier |> p_map (fun (id, q) -> { q; id })

(* ------ Match parsers ------ *)

let p_m_any_char : match_itm parser =
  p_character '.' |> p_map (fun _ -> MAnyChar)

let must_escape = String.mem {|.^$*+?()[{\||}

let p_m_char : match_itm parser =
  p_any
    [
      p_character '\\' *> p_char |> p_filter must_escape;
      p_char |> p_filter (must_escape >> not);
    ]
  |> p_map (fun ch -> MChar ch)

let p_m_char_class : match_itm parser =
  p_c_class |> p_map (fun cls -> MCharClass cls)

let p_m_char_group : match_itm parser =
  p_c_group |> p_map (fun gr -> MCharGroup gr)

let p_m_item : match_itm parser =
  p_any [ p_m_any_char; p_m_char_class; p_m_char_group; p_m_char ]

let p_match : match_typ parser =
  p_m_item <*> ??p_quantifier |> p_map (fun (itm, q) -> { itm; q })

(* ------ Expression parsers ------ *)

let rec p_regex () : regex parser =
  p_expression ()
  <*> p_zero_or_more (p_character '|' **> lazy (p_expression ()))
  |> p_map (fun (s, rst) -> s :: rst)

and p_group () : group parser =
  p_character '(' *> ??(p_str "?:")
  <**> lazy (p_regex ())
  <* p_character ')' <*> ??p_quantifier
  |> p_map (fun ((non_capt, inner), q) ->
         let cap =
           match non_capt with Some _ -> Noncapturing | None -> Capturing 0
         in
         { cap; inner; q })

and p_expression () : expression parser =
  p_any
    [
      p_group () |> p_map (fun g -> Group g);
      p_anchor |> p_map (fun a -> Anchor a);
      p_backreference |> p_map (fun b -> Backreference b);
      p_match |> p_map (fun m -> Match m);
    ]
  |> p_zero_or_more

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

let parse_regex = parse (p_regex ()) >> optimize
