let () =
  let r =
    Genre.Parser.parse {|^The end (?:hel?lo world){2,5} ma*na{3,}sd $|}
      Genre.Regex_parser.p_regex
    |> Genre.Ast.optimize
  in
  Stdio.print_s (Genre.Ast.sexp_of_regex r);
  ()
