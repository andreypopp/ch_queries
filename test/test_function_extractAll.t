Testing extractAll:

  $ ./compile_and_run '
  > let e = {%e|extractAll('\''hello world'\'', '\''[aeiou]'\'')|}
  > let () = print_endline (Ch_queries.expr_to_string e)
  > '
  >>> PREPROCESSING
  let e =
    Ch_queries.Expr.extractAll
      (Ch_queries.string "hello world")
      (Ch_queries.string "[aeiou]")
  
  let () = print_endline (Ch_queries.expr_to_string e)
  >>> RUNNING
  extractAll('hello world', '[aeiou]')
