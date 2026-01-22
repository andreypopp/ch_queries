Testing JSONHas:

  $ ./compile_and_run "
  > let e = {%e|JSONHas('{\"name\": \"John\"}', 'name')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : ('a, bool) Ch_queries.expr
  JSONHas('{"name": "John"}', 'name')
