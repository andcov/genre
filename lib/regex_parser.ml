open! Base
open Ast
open Parser

let ( >> ) f g x = g (f x)

(* Character Class parsers *)

(* exception NotImplemented *)

let p_generic_c_class (cls : string) (typ : char_class) =
  p_str ({|\|} ^ cls) |> p_map (fun _ -> typ)

let p_c_class : char_class parser =
  p_any
    [
      p_generic_c_class "w" CClassAnyWord;
      p_generic_c_class "W" CClassAnyWordInv;
      p_generic_c_class "d" CClassAnyDigit;
      p_generic_c_class "D" CClassAnyDigitInv;
    ]

(* TODO: parse escpaed chars such as '\.' *)
let p_c_char : char parser = p_char |> p_filter (Char.equal ']' >> not)

(* TODO: validate range, (i'm not sure, but i think ranges can only contain alpha-numerical chars) *)
let p_c_range : char_group_itm parser =
  p_c_char <* p_character '-' <*> p_c_char
  |> p_map (fun (l, u) -> CRange (l, u))

let p_c_group_itm : char_group_itm parser =
  p_any
    [
      p_c_class |> p_map (fun c_cls -> CClass c_cls);
      p_c_range;
      p_c_char |> p_map (fun ch -> Char ch);
    ]

let p_c_group : char_group parser =
  p_str "[" *> ??(p_str "^")
  <*> p_one_or_more p_c_group_itm
  <* p_str "]"
  |> p_map (fun (neg_opt, grp_itm_list) ->
         (Option.is_some neg_opt, grp_itm_list))

(* Quantifier parsers *)
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

(* Anchor parsers *)
let p_a_word_bnd : anchor parser =
  p_str {|\b|} |> p_map (fun _ -> AWordBoundary)

let p_a_non_word_bnd : anchor parser =
  p_str {|\B|} |> p_map (fun _ -> ANonWordBoundary)

let p_a_start_of_str : anchor parser =
  p_str "^" |> p_map (fun _ -> AStartOfString)

let p_a_end_of_str : anchor parser = p_str "$" |> p_map (fun _ -> AEndOfString)

let p_anchor : anchor parser =
  p_any [ p_a_word_bnd; p_a_non_word_bnd; p_a_start_of_str; p_a_end_of_str ]
