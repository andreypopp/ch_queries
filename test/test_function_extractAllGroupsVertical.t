Testing extractAllGroupsVertical:

  $ ./compile_and_run '
  > let e = {%e|extractAllGroupsVertical('\''abc=111, def=222, ghi=333'\'', '\''([a-z]+)=([0-9]+)'\'')|}
  > let () = print_endline (Ch_queries.expr_to_string e)
  > '
  >>> PREPROCESSING
  let e =
    Ch_queries.Expr.extractAllGroupsVertical
      (Ch_queries.string "abc=111, def=222, ghi=333")
      (Ch_queries.string "([a-z]+)=([0-9]+)")
  
  let () = print_endline (Ch_queries.expr_to_string e)
  >>> RUNNING
  extractAllGroupsVertical('abc=111, def=222, ghi=333', '([a-z]+)=([0-9]+)')
