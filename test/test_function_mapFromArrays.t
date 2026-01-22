Testing mapFromArrays:

  $ ./compile_and_run "
  > let e = {%e|mapFromArrays(['a', 'b'], [1, 2])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, string, Ch_queries.non_null, int Ch_queries.number)
     Ch_queries.map)
    Ch_queries.expr
  mapFromArrays(['a', 'b'], [1, 2])
