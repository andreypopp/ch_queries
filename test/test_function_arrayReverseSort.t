Testing arrayReverseSort:

  $ ./compile_and_run "
  > let e = {%e|arrayReverseSort([1, 2, 3])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, int Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  arrayReverseSort([1, 2, 3])

Testing arrayReverseSort with lambda:

  $ ./compile_and_run "
  > let e = {%e|arrayReverseSort(x -> x * 2, [1, 2, 3])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, int Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  arrayReverseSort((x -> (x * 2)), [1, 2, 3])
