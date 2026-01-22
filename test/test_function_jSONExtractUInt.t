Testing JSONExtractUInt:

  $ ./compile_and_run "
  > let e = {%e|JSONExtractUInt('{\"count\": 42}', 'count')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : ('a, Ch_queries.uint64 Ch_queries.number) Ch_queries.expr
  JSONExtractUInt('{"count": 42}', 'count')
