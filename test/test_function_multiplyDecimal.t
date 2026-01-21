Testing multiplyDecimal:

  $ ./compile_and_run "
  > let e = {%e|multiplyDecimal(1, 2)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  multiplyDecimal(1, 2)

Testing multiplyDecimal with result_scale:

  $ ./compile_and_run "
  > let e = {%e|multiplyDecimal(1, 2, 4)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  multiplyDecimal(1, 2, 4)
