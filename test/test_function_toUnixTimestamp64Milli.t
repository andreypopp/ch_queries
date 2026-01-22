Testing toUnixTimestamp64Milli:

  $ ./compile_and_run "
  > let e = {%e|toUnixTimestamp64Milli(toDateTime64('2024-03-15 10:30:45.123'))|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int64 Ch_queries.number) Ch_queries.expr
  toUnixTimestamp64Milli(toDateTime64('2024-03-15 10:30:45.123'))
