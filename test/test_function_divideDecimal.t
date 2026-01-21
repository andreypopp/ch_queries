Testing divideDecimal:

  $ ./compile_and_run "
  > let e = {%e|divideDecimal(1, 2)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, '_weak1 Ch_queries.number) Ch_queries.expr
  divideDecimal(1, 2)

Testing divideDecimal with result_scale:

  $ ./compile_and_run "
  > let e = {%e|divideDecimal(1, 2, 4)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, '_weak1 Ch_queries.number) Ch_queries.expr
  divideDecimal(1, 2, 4)
