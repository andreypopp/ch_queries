Testing multiSearchAnyCaseInsensitiveUTF8:

  $ ./compile_and_run "
  > let e = {%e|multiSearchAnyCaseInsensitiveUTF8('hello world', ['WORLD', 'FOO'])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, bool) Ch_queries.expr
  multiSearchAnyCaseInsensitiveUTF8('hello world', ['WORLD', 'FOO'])
