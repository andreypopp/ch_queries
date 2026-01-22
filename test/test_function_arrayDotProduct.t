Testing arrayDotProduct:

  $ ./compile_and_run "
  > let e = {%e|arrayDotProduct([1, 2, 3], [4, 5, 6])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  arrayDotProduct([1, 2, 3], [4, 5, 6])
