Testing parseDateTimeBestEffort:

  $ ./compile_and_run "
  > let e = {%e|parseDateTimeBestEffort('2023-01-15 10:30:00')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, Ch_queries.datetime) Ch_queries.expr
  parseDateTimeBestEffort('2023-01-15 10:30:00')
