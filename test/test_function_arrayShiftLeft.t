Testing arrayShiftLeft:

  $ ./compile_and_run "
  > let e = {%e|arrayShiftLeft([1, 2, 3], 2)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, int Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  arrayShiftLeft([1, 2, 3], 2)

Testing arrayShiftLeft with default:

  $ ./compile_and_run "
  > let e = {%e|arrayShiftLeft([1, 2, 3], 2, 0)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, int Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  arrayShiftLeft([1, 2, 3], 2, 0)
