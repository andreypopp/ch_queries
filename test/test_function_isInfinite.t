Testing isInfinite:

  $ ./compile_and_run "
  > let e = {%e|isInfinite(1.0)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, bool) Ch_queries.expr
  isInfinite(1.)
