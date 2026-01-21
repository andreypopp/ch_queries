Testing bitShiftRight:

  $ ./compile_and_run "
  > let e = {%e|bitShiftRight(40, 3)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  bitShiftRight(40, 3)
