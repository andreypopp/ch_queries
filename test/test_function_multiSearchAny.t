Testing multiSearchAny:

  $ ./compile_and_run "
  > let e = {%e|multiSearchAny('hello world', ['foo', 'world'])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, bool) Ch_queries.expr
  multiSearchAny('hello world', ['foo', 'world'])
