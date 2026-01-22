Testing version:

  $ ./compile_and_run "
  > let e = {%e|version()|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  version()
