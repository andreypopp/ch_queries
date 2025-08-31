

type ascription for expressions:
  $ ./compile_and_run '
  > let x = {%e|1::Int32|}
  > '
  >>> PREPROCESSING
  let x =
    (Ch_queries.int 1
      : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr)
  >>> RUNNING
