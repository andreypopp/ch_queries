Testing timeSlots:

  $ ./compile_and_run "
  > let e = {%e|timeSlots(toDateTime('2024-01-15 10:00:00'), 3600)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, Ch_queries.datetime0 Ch_queries.timestamp)
     Ch_queries.array)
    Ch_queries.expr
  timeSlots(toDateTime('2024-01-15 10:00:00'), 3600)

Testing timeSlots with size:

  $ ./compile_and_run "
  > let e = {%e|timeSlots(toDateTime('2024-01-15 10:00:00'), 3600, 900)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, Ch_queries.datetime0 Ch_queries.timestamp)
     Ch_queries.array)
    Ch_queries.expr
  timeSlots(toDateTime('2024-01-15 10:00:00'), 3600, 900)
