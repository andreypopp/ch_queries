Testing splitByWhitespace:

  $ ./compile_and_run '
  > let e = {%e|splitByWhitespace('\''hello world foo bar'\'')|}
  > let () = print_endline (Ch_queries.expr_to_string e)
  > '
  >>> PREPROCESSING
  let e =
    Ch_queries.Expr.splitByWhitespace (Ch_queries.string "hello world foo bar")
  
  let () = print_endline (Ch_queries.expr_to_string e)
  >>> RUNNING
  splitByWhitespace('hello world foo bar')
