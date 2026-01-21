Testing divideOrNull:

  $ ./compile_and_run "
  > let e = {%e|divideOrNull(1, 2)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.null, float Ch_queries.number) Ch_queries.expr
  divideOrNull(1, 2)
