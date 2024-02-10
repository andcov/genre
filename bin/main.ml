let () =
  let r = Genre.Regex_parser.parse_regex {|ab?|b[2-9]c|} in
  let nfa = Genre.Nfa.of_regex r in
  let dot = Genre.Nfa.to_dot nfa in
  Printf.fprintf (open_out "nfa.dot") "%s" dot;
  Stdio.print_s (Genre.Ast.sexp_of_regex r);
  Stdio.print_s (Genre.Nfa.sexp_of_t nfa);
  ()
