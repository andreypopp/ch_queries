Testing JSONExtractString:

  $ ./compile_and_run "
  > let e = {%e|JSONExtractString('{\"name\": \"John\"}', 'name')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : ('a, string) Ch_queries.expr
  JSONExtractString('{"name": "John"}', 'name')
