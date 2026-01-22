Testing toStartOfSecond:

  $ ./compile_and_run "
  > let e = {%e|toStartOfSecond(toDateTime('2024-03-15 10:30:45'))|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null, Ch_queries.datetime0 Ch_queries.timestamp)
    Ch_queries.expr
  toStartOfSecond(toDateTime('2024-03-15 10:30:45'))
