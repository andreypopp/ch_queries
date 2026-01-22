Testing multiSearchAnyUTF8:

  $ ./compile_and_run "
  > let e = {%e|multiSearchAnyUTF8('hello world', ['world', 'foo'])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, bool) Ch_queries.expr
  multiSearchAnyUTF8('hello world', ['world', 'foo'])
