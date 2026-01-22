Testing notILike:

  $ ./compile_and_run '
  > let e = {%e|notILike('\''Hello World'\'', '\''%WORLD%'\'')|}
  > let () = print_endline (Ch_queries.expr_to_string e)
  > '
  >>> PREPROCESSING
  let e =
    Ch_queries.Expr.notILike
      (Ch_queries.string "Hello World")
      (Ch_queries.string "%WORLD%")
  
  let () = print_endline (Ch_queries.expr_to_string e)
  >>> RUNNING
  notILike('Hello World', '%WORLD%')
