Testing rowNumberInAllBlocks:

  $ ./compile_and_run "
  > let e = {%e|rowNumberInAllBlocks()|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int64 Ch_queries.number) Ch_queries.expr
  rowNumberInAllBlocks()
