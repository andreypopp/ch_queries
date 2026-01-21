Testing avg2:

  $ ./compile_and_run '
  > let e = {%e|avg2(1, 2)|}
  > let () = print_endline (Ch_queries.expr_to_string e)
  > '
  >>> PREPROCESSING
  let e = Ch_queries.Expr.avg2 (Ch_queries.int 1) (Ch_queries.int 2)
  let () = print_endline (Ch_queries.expr_to_string e)
  >>> RUNNING
  avg2(1, 2)
