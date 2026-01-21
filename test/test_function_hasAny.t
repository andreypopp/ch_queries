Testing hasAny:

  $ ./compile_and_run "
  > let e = {%e|hasAny([1, 2, 3], [2, 4])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, bool) Ch_queries.expr
  hasAny([1, 2, 3], [2, 4])
