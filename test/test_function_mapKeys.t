Testing mapKeys:

  $ ./compile_and_run "
  > let e = {%e|mapKeys(map('a', 1, 'b', 2))|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null, (Ch_queries.non_null, string) Ch_queries.array)
    Ch_queries.expr
  mapKeys(map('a', 1, 'b', 2))
