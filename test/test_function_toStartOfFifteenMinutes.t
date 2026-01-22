Testing toStartOfFifteenMinutes:

  $ ./compile_and_run "
  > let e = {%e|toStartOfFifteenMinutes(toDateTime('2024-01-15 10:37:00'))|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null, Ch_queries.datetime0 Ch_queries.timestamp)
    Ch_queries.expr
  toStartOfFifteenMinutes(toDateTime('2024-01-15 10:37:00'))
