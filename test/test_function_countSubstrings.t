Testing countSubstrings:

  $ ./compile_and_run '
  > let e = {%e|countSubstrings('\''hello world'\'', '\''o'\'')|}
  > let () = print_endline (Ch_queries.expr_to_string e)
  > '
  >>> PREPROCESSING
  let e =
    Ch_queries.Expr.countSubstrings
      (Ch_queries.string "hello world")
      (Ch_queries.string "o")
  
  let () = print_endline (Ch_queries.expr_to_string e)
  >>> RUNNING
  countSubstrings('hello world', 'o')
