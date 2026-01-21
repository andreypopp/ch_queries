Testing hasSubstr:

  $ ./compile_and_run "
  > let e = {%e|hasSubstr([1, 2, 3, 4], [2, 3])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, bool) Ch_queries.expr
  hasSubstr([1, 2, 3, 4], [2, 3])
