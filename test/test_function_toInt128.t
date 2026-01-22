Testing toInt128:

  $ ./compile_and_run "
  > let e = {%e|toInt128(12345)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int64 Ch_queries.number) Ch_queries.expr
  toInt128(12345)
