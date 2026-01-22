Testing toUInt16:

  $ ./compile_and_run "
  > let e = {%e|toUInt16(123)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  toUInt16(123)
