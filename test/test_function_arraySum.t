Testing arraySum:

  $ ./compile_and_run "
  > let e = {%e|arraySum([1, 2, 3])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  arraySum([1, 2, 3])

Testing arraySum with lambda:

  $ ./compile_and_run "
  > let e = {%e|arraySum(x -> x * 2, [1, 2, 3])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  arraySum((x -> (x * 2)), [1, 2, 3])
