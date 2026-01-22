Testing formatDateTime:

  $ ./compile_and_run "
  > let e = {%e|formatDateTime(toDateTime('2024-01-15 10:30:00'), '%Y-%m-%d')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  formatDateTime(toDateTime('2024-01-15 10:30:00'), '%Y-%m-%d')

Testing formatDateTime with timezone:

  $ ./compile_and_run "
  > let e = {%e|formatDateTime(toDateTime('2024-01-15 10:30:00'), '%Y-%m-%d %H:%M:%S', 'UTC')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  formatDateTime(toDateTime('2024-01-15 10:30:00'), '%Y-%m-%d %H:%M:%S', 'UTC')
