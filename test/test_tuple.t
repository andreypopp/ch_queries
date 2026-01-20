
tuple2:
  $ ./compile_and_run '
  > let x = [%e "tuple(1, 2)"]
  > let () = print_endline (Ch_queries.expr_to_string x)
  > ' --run-only
  >>> RUNNING
  tuple(1, 2)

tuple3:
  $ ./compile_and_run '
  > let x = [%e "tuple(1, 2, 3)"]
  > let () = print_endline (Ch_queries.expr_to_string x)
  > ' --run-only
  >>> RUNNING
  tuple(1, 2, 3)

tuple4:
  $ ./compile_and_run '
  > let x = [%e "tuple(1, 2, 3, 4)"]
  > let () = print_endline (Ch_queries.expr_to_string x)
  > ' --run-only
  >>> RUNNING
  tuple(1, 2, 3, 4)

tuple with wrong arity:
  $ ./compile_and_run '
  > let x = [%e "tuple(1)"]
  > ' 2>&1 | grep -v File
  >>> PREPROCESSING
  Error: tuple(...) requires 2, 3, or 4 arguments
