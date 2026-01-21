Testing multiply:

  $ ./compile_and_run "
  > let e = {%e|multiply(10, 20)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  10 * 20
