Testing replaceRegexpOne:

  $ ./compile_and_run '
  > let e = {%e|replaceRegexpOne('\''hello world'\'', '\''o'\'', '\''0'\'')|}
  > let () = print_endline (Ch_queries.expr_to_string e)
  > '
  >>> PREPROCESSING
  let e =
    Ch_queries.Expr.replaceRegexpOne
      (Ch_queries.string "hello world")
      (Ch_queries.string "o") (Ch_queries.string "0")
  
  let () = print_endline (Ch_queries.expr_to_string e)
  >>> RUNNING
  replaceRegexpOne('hello world', 'o', '0')
