Testing arrayReduce:

  $ ./compile_and_run "
  > let e = {%e|arrayReduce('max', [1, 2, 3, 4, 5])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : ('a, 'b) Ch_queries.expr
  arrayReduce('max', [1, 2, 3, 4, 5])
