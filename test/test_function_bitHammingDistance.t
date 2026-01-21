Testing bitHammingDistance:

  $ ./compile_and_run "
  > let e = {%e|bitHammingDistance(5, 3)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  bitHammingDistance(5, 3)
