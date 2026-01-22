Testing queryString:

  $ ./compile_and_run "
  > let e = {%e|queryString('https://example.com/page?foo=bar&baz=qux')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  queryString('https://example.com/page?foo=bar&baz=qux')
