Testing trunc:

  $ ./compile_and_run "
  > let e = {%e|trunc(3.14)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, float Ch_queries.number) Ch_queries.expr
  trunc(3.14)

Testing trunc with decimal places:

  $ ./compile_and_run "
  > let e = {%e|trunc(3.14159, 2)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, float Ch_queries.number) Ch_queries.expr
  trunc(3.14159, 2)
