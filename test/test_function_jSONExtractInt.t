Testing JSONExtractInt:

  $ ./compile_and_run "
  > let e = {%e|JSONExtractInt('{\"count\": 42}', 'count')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : ('a, int64 Ch_queries.number) Ch_queries.expr
  JSONExtractInt('{"count": 42}', 'count')
