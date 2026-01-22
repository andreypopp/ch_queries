Testing JSONExtractBool:

  $ ./compile_and_run "
  > let e = {%e|JSONExtractBool('{\"active\": true}', 'active')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : ('a, bool) Ch_queries.expr
  JSONExtractBool('{"active": true}', 'active')
