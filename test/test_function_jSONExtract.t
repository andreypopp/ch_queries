Testing JSONExtract:

  $ ./compile_and_run "
  > let e = {%e|JSONExtract('{\"name\": \"John\"}', 'name', 'String')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : ('a, 'b) Ch_queries.expr
  JSONExtract('{"name": "John"}', 'name', 'String')
