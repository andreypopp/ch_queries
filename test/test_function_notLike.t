Testing notLike:

  $ ./compile_and_run '
  > let e = {%e|notLike('\''Hello World'\'', '\''%world%'\'')|}
  > let () = print_endline (Ch_queries.expr_to_string e)
  > '
  >>> PREPROCESSING
  let e =
    Ch_queries.Expr.notLike
      (Ch_queries.string "Hello World")
      (Ch_queries.string "%world%")
  
  let () = print_endline (Ch_queries.expr_to_string e)
  >>> RUNNING
  notLike('Hello World', '%world%')
