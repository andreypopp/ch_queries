Testing isNaN:

  $ ./compile_and_run "
  > let e = {%e|isNaN(1.0)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, bool) Ch_queries.expr
  isNaN(1.)
