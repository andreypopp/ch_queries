Basic unary minus:
  $ ./compile_and_run '
  > let x = [%e "-5"]
  > '
  >>> PREPROCESSING
  let x = Ch_queries.Expr.neg (Ch_queries.int 5)
  >>> RUNNING

Unary minus precedence with addition:
  $ ./compile_and_run '
  > let x = [%e "-5 + 3"]
  > '
  >>> PREPROCESSING
  let x =
    Ch_queries.Expr.( + )
      (Ch_queries.Expr.neg (Ch_queries.int 5))
      (Ch_queries.int 3)
  >>> RUNNING

Unary minus with parentheses:
  $ ./compile_and_run '
  > let x = [%e "-(5 + 3)"]
  > '
  >>> PREPROCESSING
  let x =
    Ch_queries.Expr.neg
      (Ch_queries.Expr.( + ) (Ch_queries.int 5) (Ch_queries.int 3))
  >>> RUNNING

Double unary minus:
  $ ./compile_and_run '
  > let x = [%e "--5"]
  > '
  >>> PREPROCESSING
  let x = Ch_queries.Expr.neg (Ch_queries.Expr.neg (Ch_queries.int 5))
  >>> RUNNING

Unary minus with multiplication:
  $ ./compile_and_run '
  > let x = [%e "-5 * 3"]
  > '
  >>> PREPROCESSING
  let x =
    Ch_queries.Expr.( * )
      (Ch_queries.Expr.neg (Ch_queries.int 5))
      (Ch_queries.int 3)
  >>> RUNNING
