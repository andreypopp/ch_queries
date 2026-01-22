Testing toUInt64OrNull:

  $ ./compile_and_run "
  > let e = {%e|toUInt64OrNull('123')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.null, Ch_queries.uint64 Ch_queries.number) Ch_queries.expr
  toUInt64OrNull('123')
