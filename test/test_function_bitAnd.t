Testing bitAnd:

  $ ./compile_and_run "
  > let e = {%e|bitAnd(5, 3)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, '_weak1 Ch_queries.number) Ch_queries.expr
  bitAnd(5, 3)
