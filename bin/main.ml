let () =
  let r = Genre.Parser.parse {| [^i*&2@]*? |} Genre.Regex_parser.p_regex in
  Stdio.print_s (Genre.Ast.sexp_of_regex r);
  ()
