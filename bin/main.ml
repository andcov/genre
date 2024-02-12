let () =
  let r = Genre.Regex_parser.parse_regex {|go+?gle|} in
  (* Stdio.print_s (Genre.Ast.sexp_of_regex r); *)
  let nfa = Genre.Nfa.of_regex r in

  (match Genre.Nfa.match_string nfa "goooogle" with
  | Ok i -> Stdio.printf "Match: %d\n" i
  | Error e -> Stdio.printf "No match: %s\n" e);

  let dot = Genre.Nfa.to_dot nfa in
  Printf.fprintf (open_out "nfa.dot") "%s" dot;
  (* Stdio.print_s (Genre.Nfa.sexp_of_t nfa); *)
  ()
