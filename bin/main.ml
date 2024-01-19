let () =
  let r =
    Genre.Parser.parse
      {|^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$|}
      (Genre.Regex_parser.p_regex ())
    |> Genre.Ast.optimize_chars
  in
  (* Genre.Ast.print_regex r; *)
  Stdio.print_s (Genre.Ast.sexp_of_regex r);
  ()
