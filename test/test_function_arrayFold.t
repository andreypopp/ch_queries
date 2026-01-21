Testing arrayFold:

  $ ./compile_and_run "
  > let e = {%e|arrayFold((acc, x) -> acc + x, [1, 2, 3, 4], 0)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  arrayFold(((acc, x) -> (acc + x)), [1, 2, 3, 4], 0)
