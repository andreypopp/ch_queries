Testing toTypeName:

  $ ./compile_and_run "
  > let e = {%e|toTypeName(123)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  toTypeName(123)
