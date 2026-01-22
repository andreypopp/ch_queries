Testing toYearWeek:

  $ ./compile_and_run "
  > let e = {%e|toYearWeek(toDate('2024-06-15'))|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  toYearWeek(toDate('2024-06-15'))

Testing toYearWeek with mode:

  $ ./compile_and_run "
  > let e = {%e|toYearWeek(toDate('2024-06-15'), 1)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  toYearWeek(toDate('2024-06-15'), 1)

Testing toYearWeek with mode and timezone:

  $ ./compile_and_run "
  > let e = {%e|toYearWeek(toDate('2024-06-15'), 1, 'Europe/Berlin')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  toYearWeek(toDate('2024-06-15'), 1, 'Europe/Berlin')
