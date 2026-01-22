Testing pow:

  $ ./compile_and_run "
  > let e = {%e|pow(2, 3)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, float Ch_queries.number) Ch_queries.expr
  pow(2, 3)
