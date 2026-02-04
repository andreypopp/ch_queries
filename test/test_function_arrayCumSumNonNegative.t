Testing arrayCumSumNonNegative:

  $ ./compile_and_run "
  > let e = {%e|arrayCumSumNonNegative([1, -2, 3])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, int Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  arrayCumSumNonNegative([1, negate(2), 3])

Testing arrayCumSumNonNegative with lambda:

  $ ./compile_and_run "
  > let e = {%e|arrayCumSumNonNegative(x -> x * 2, [1, -2, 3])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, int Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  arrayCumSumNonNegative((x -> (x * 2)), [1, negate(2), 3])
