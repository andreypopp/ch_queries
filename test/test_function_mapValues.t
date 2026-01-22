Testing mapValues:

  $ ./compile_and_run "
  > let e = {%e|mapValues(map('a', 1, 'b', 2))|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, int Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  mapValues(map('a', 1, 'b', 2))
