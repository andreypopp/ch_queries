Testing toMonth:

  $ ./compile_and_run "
  > let e = {%e|toMonth(toDate('2024-03-15'))|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  toMonth(toDate('2024-03-15'))
