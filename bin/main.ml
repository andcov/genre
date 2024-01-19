let () =
  let r =
    Genre.Parser.parse {|(foo (nar) (bix)) (bax)|(bux) (nax (iop))|}
      (Genre.Regex_parser.p_regex ())
    |> Genre.Ast.optimize
  in
  (* Genre.Ast.print_regex r; *)
  Stdio.print_s (Genre.Ast.sexp_of_regex r);
  ()
