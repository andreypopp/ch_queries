Testing multiMatchAny:

  $ ./compile_and_run "
  > let e = {%e|multiMatchAny('hello world', ['foo.*', 'wor.d'])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, bool) Ch_queries.expr
  multiMatchAny('hello world', ['foo.*', 'wor.d'])
