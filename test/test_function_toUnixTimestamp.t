Testing toUnixTimestamp:

  $ ./compile_and_run "
  > let e = {%e|toUnixTimestamp(toDateTime('2024-03-15 10:30:45'))|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  toUnixTimestamp(toDateTime('2024-03-15 10:30:45'))
