Testing lower:

  $ ./compile_and_run "
  > let e = {%e|lower('HELLO')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  lower('HELLO')
