Testing toDateTime64:

  $ ./compile_and_run "
  > let e = {%e|toDateTime64('2024-01-15 10:30:00')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, Ch_queries.datetime64) Ch_queries.expr
  toDateTime64('2024-01-15 10:30:00')
