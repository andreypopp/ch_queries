Testing mapContainsKey:

  $ ./compile_and_run "
  > let e = {%e|mapContainsKey(map('a', 1, 'b', 2), 'a')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, bool) Ch_queries.expr
  mapContainsKey(map('a', 1, 'b', 2), 'a')
