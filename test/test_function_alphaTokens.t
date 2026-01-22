Testing alphaTokens:

  $ ./compile_and_run '
  > let e = {%e|alphaTokens('\''abc123def456'\'')|}
  > let () = print_endline (Ch_queries.expr_to_string e)
  > '
  >>> PREPROCESSING
  let e = Ch_queries.Expr.alphaTokens (Ch_queries.string "abc123def456")
  let () = print_endline (Ch_queries.expr_to_string e)
  >>> RUNNING
  alphaTokens('abc123def456')
