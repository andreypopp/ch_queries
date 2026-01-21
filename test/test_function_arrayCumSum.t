Testing arrayCumSum:

  $ ./compile_and_run "
  > let e = {%e|arrayCumSum([1, 2, 3])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, int Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  arrayCumSum([1, 2, 3])

Testing arrayCumSum with lambda:

  $ ./compile_and_run "
  > let e = {%e|arrayCumSum(x -> x * 2, [1, 2, 3])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, int Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  arrayCumSum((x -> (x * 2)), [1, 2, 3])
