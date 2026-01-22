Testing arrayZipUnaligned:

  $ ./compile_and_run "
  > let e = {%e|arrayZipUnaligned([1, 2, 3], [4, 5])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null,
      ((Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr,
       (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr)
      Ch_queries.tuple2)
     Ch_queries.array)
    Ch_queries.expr
  arrayZipUnaligned([1, 2, 3], [4, 5])
