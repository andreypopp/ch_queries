Testing arrayUnion:

  $ ./compile_and_run "
  > let e = {%e|arrayUnion([1, 2], [1, 3], [1, 4])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, int Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  arrayUnion([1, 2], [1, 3], [1, 4])
