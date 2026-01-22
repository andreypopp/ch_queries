Testing toIntervalNanosecond:

  $ ./compile_and_run "
  > let e = {%e|toIntervalNanosecond(1000000)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, Ch_queries.interval) Ch_queries.expr
  toIntervalNanosecond(1000000)
