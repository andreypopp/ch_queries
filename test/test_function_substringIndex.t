Testing substringIndex:

  $ ./compile_and_run "
  > let e = {%e|substringIndex('www.clickhouse.com', '.', 2)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  substringIndex('www.clickhouse.com', '.', 2)
