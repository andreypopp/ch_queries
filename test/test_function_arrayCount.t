Testing arrayCount:

  $ ./compile_and_run "
  > let e = {%e|arrayCount([1, 0, 2, 0, 3])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  arrayCount([1, 0, 2, 0, 3])

Testing arrayCount with lambda:

  $ ./compile_and_run "
  > let e = {%e|arrayCount(x -> x > 1, [1, 2, 3])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  arrayCount((x -> (x > 1)), [1, 2, 3])
