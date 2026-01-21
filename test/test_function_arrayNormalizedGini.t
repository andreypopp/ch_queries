Testing arrayNormalizedGini:

  $ ./compile_and_run "
  > let e = {%e|arrayNormalizedGini([0.1, 0.4, 0.35, 0.8], [0, 0, 1, 1])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     ((Ch_queries.non_null, float Ch_queries.number) Ch_queries.expr,
      (Ch_queries.non_null, float Ch_queries.number) Ch_queries.expr,
      (Ch_queries.non_null, float Ch_queries.number) Ch_queries.expr)
     Ch_queries.tuple3)
    Ch_queries.expr
  arrayNormalizedGini([0.1, 0.4, 0.35, 0.8], [0, 0, 1, 1])
