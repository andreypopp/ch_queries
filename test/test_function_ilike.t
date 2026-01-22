Testing ilike:

  $ ./compile_and_run '
  > let e = {%e|ilike('\''Hello World'\'', '\''%WORLD%'\'')|}
  > let () = print_endline (Ch_queries.expr_to_string e)
  > '
  >>> PREPROCESSING
  let e =
    Ch_queries.Expr.ilike
      (Ch_queries.string "Hello World")
      (Ch_queries.string "%WORLD%")
  
  let () = print_endline (Ch_queries.expr_to_string e)
  >>> RUNNING
  ilike('Hello World', '%WORLD%')
