Testing round:

  $ ./compile_and_run "
  > let e = {%e|round(3.14)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, float Ch_queries.number) Ch_queries.expr
  round(3.14)

Testing round with decimal places:

  $ ./compile_and_run "
  > let e = {%e|round(3.14159, 2)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, float Ch_queries.number) Ch_queries.expr
  round(3.14159, 2)
