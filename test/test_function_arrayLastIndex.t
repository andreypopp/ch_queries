Testing arrayLastIndex:

  $ ./compile_and_run "
  > let e = {%e|arrayLastIndex(x -> x > 2, [1, 2, 3, 4])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  arrayLastIndex((x -> (x > 2)), [1, 2, 3, 4])
