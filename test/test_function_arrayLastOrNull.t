Testing arrayLastOrNull:

  $ ./compile_and_run "
  > let e = {%e|arrayLastOrNull(x -> x > 2, [1, 2, 3, 4])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.null, int Ch_queries.number) Ch_queries.expr
  arrayLastOrNull((x -> (x > 2)), [1, 2, 3, 4])
