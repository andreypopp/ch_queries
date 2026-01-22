Testing toIntervalSecond:

  $ ./compile_and_run "
  > let e = {%e|toIntervalSecond(60)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, Ch_queries.interval) Ch_queries.expr
  toIntervalSecond(60)
