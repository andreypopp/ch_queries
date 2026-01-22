Testing toBool:

  $ ./compile_and_run "
  > let e = {%e|toBool('true')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, bool) Ch_queries.expr
  toBool('true')
