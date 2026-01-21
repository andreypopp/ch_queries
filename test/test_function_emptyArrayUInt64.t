Testing emptyArrayUInt64:

  $ ./compile_and_run "
  > let e = {%e|emptyArrayUInt64()|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, Ch_queries.uint64 Ch_queries.number)
     Ch_queries.array)
    Ch_queries.expr
  emptyArrayUInt64()
