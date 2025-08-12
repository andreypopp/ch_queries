NOT operator:
  $ ./compile_and_run '
  > let x = [%e "NOT TRUE"]
  > '
  >>> PREPROCESSING
  let x = Ch_queries.Expr.not_ (Ch_queries.bool true)
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "NOT FALSE"]
  > '
  >>> PREPROCESSING
  let x = Ch_queries.Expr.not_ (Ch_queries.bool false)
  >>> RUNNING

NOT with precedence:
  $ ./compile_and_run '
  > let x = [%e "NOT TRUE AND FALSE"]
  > '
  >>> PREPROCESSING
  let x =
    Ch_queries.Expr.( && )
      (Ch_queries.Expr.not_ (Ch_queries.bool true))
      (Ch_queries.bool false)
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "NOT (TRUE AND FALSE)"]
  > '
  >>> PREPROCESSING
  let x =
    Ch_queries.Expr.not_
      (Ch_queries.Expr.( && ) (Ch_queries.bool true) (Ch_queries.bool false))
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "TRUE OR NOT FALSE"]
  > '
  >>> PREPROCESSING
  let x =
    Ch_queries.Expr.( || ) (Ch_queries.bool true)
      (Ch_queries.Expr.not_ (Ch_queries.bool false))
  >>> RUNNING
