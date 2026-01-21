Testing intDivOrNull:

  $ ./compile_and_run "
  > let e = {%e|intDivOrNull(10, 3)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.null, int Ch_queries.number) Ch_queries.expr
  intDivOrNull(10, 3)
