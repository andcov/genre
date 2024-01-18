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
  |> p_map (fun (neg_opt, grp_itm_list) ->
         (Option.is_some neg_opt, grp_itm_list))

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
         (QRange q, Option.is_some lzy))

let p_q_one_char (ch : char) (typ : quantifier_typ) =
  p_character ch *> ??(p_character '?')
  |> p_map (fun lzy -> (typ, Option.is_some lzy))

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

let p_backreference : subexpression parser =
  p_str {|\|} *> p_number |> p_map (fun i -> Backreference i)

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

let p_match : match_typ parser = p_m_item <*> ??p_quantifier

(* ------ Group parsers ------ *)

let rec p_expression () : expression parser =
  p_subexpressions ()
  <*> ??(p_character '|' **> lazy (p_expression ()))
  |> p_map (fun (sub_e, rst) ->
         match rst with None -> Leaf sub_e | Some e -> Node (sub_e, e))

and p_group () : group parser =
  p_character '(' *> ??(p_str "?:")
  <**> lazy (p_expression ())
  <* p_character ')' <*> ??p_quantifier
  |> p_map (fun ((non_capt, exp), quant) ->
         (Option.is_some non_capt, exp, quant))

and p_subexpressions () : subexpression list parser =
  p_any
    [
      p_group () |> p_map (fun g -> Group g);
      p_anchor |> p_map (fun a -> Anchor a);
      p_backreference;
      p_match |> p_map (fun m -> Match m);
    ]
  |> p_one_or_more

let p_regex : regex parser =
  ??(p_anchor |> p_filter (phys_equal AStartOfString))
  |> p_map Option.is_some <*> p_expression ()
