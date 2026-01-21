Testing arrayMax:

  $ ./compile_and_run "
  > let e = {%e|arrayMax([1, 2, 3])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  arrayMax([1, 2, 3])

Testing arrayMax with lambda:

  $ ./compile_and_run "
  > let e = {%e|arrayMax(x -> x * 2, [1, 2, 3])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  arrayMax((x -> (x * 2)), [1, 2, 3])
