Testing arrayRemove:

  $ ./compile_and_run "
  > let e = {%e|arrayRemove([1, 2, 3, 2, 4], 2)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, int Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  arrayRemove([1, 2, 3, 2, 4], 2)
