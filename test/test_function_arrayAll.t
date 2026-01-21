Testing arrayAll:

  $ ./compile_and_run "
  > let e = {%e|arrayAll(x -> x > 0, [1, 2, 3])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  arrayAll((x -> (x > 0)), [1, 2, 3])
