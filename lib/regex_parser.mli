open Ast
open Parser

(* Quantifier parsers *)
val p_q_range : quantifier parser
val p_q_zro_or_mor : quantifier parser
val p_q_one_or_mor : quantifier parser
val p_q_zro_or_one : quantifier parser
val p_quantifier : quantifier parser

(* Charachter Class parsers *)
val p_c_class : char_class parser
val p_c_group : char_group parser

(* Anchor parsers *)
val p_a_word_bnd : anchor parser
val p_a_non_word_bnd : anchor parser
val p_a_start_of_str : anchor parser
val p_a_end_of_str : anchor parser
val p_anchor : anchor parser

(* Backreference parser *)
val p_backreference : subexpression parser

(* Match parsers *)
val p_m_any_char : match_itm parser
val p_m_char : match_itm parser
val p_m_char_class : match_itm parser
val p_m_item : match_itm parser
val p_match : match_typ parser

(* Expression parsers *)
val p_group : unit -> group parser
val p_expression : unit -> expression parser
val p_regex : unit -> regex parser
