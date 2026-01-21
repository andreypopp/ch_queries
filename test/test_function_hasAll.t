Testing hasAll:

  $ ./compile_and_run "
  > let e = {%e|hasAll([1, 2, 3, 4], [1, 3])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, bool) Ch_queries.expr
  hasAll([1, 2, 3, 4], [1, 3])
