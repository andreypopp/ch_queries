Testing arrayResize:

  $ ./compile_and_run "
  > let e = {%e|arrayResize([1, 2, 3], 5)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, int Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  arrayResize([1, 2, 3], 5)

Testing arrayResize with extender:

  $ ./compile_and_run "
  > let e = {%e|arrayResize([1, 2, 3], 5, 0)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, int Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  arrayResize([1, 2, 3], 5, 0)
