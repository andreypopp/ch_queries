Testing arrayMin:

  $ ./compile_and_run "
  > let e = {%e|arrayMin([1, 2, 3])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  arrayMin([1, 2, 3])

Testing arrayMin with lambda:

  $ ./compile_and_run "
  > let e = {%e|arrayMin(x -> x * 2, [1, 2, 3])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  arrayMin((x -> (x * 2)), [1, 2, 3])
