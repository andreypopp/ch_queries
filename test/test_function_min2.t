Testing min2:

  $ ./compile_and_run "
  > let e = {%e|min2(10, 20)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  min2(10, 20)
