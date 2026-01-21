Testing arrayReduceInRanges:

  $ ./compile_and_run "
  > let e = {%e|arrayReduceInRanges('sum', [tuple(1, 5), tuple(2, 3)], [1, 2, 3, 4, 5])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : ('a, 'b) Ch_queries.expr
  arrayReduceInRanges('sum', [tuple(1, 5), tuple(2, 3)], [1, 2, 3, 4, 5])
