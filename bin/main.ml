let () =
  let r =
    Genre.Regex_parser.parse_regex {|(foo (nar) (bix)) (bax)|(bux) (nax (iop))|}
  in
  Stdio.print_s (Genre.Ast.sexp_of_regex r);
  ()
