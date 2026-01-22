Testing replaceAll:

  $ ./compile_and_run "
  > let e = {%e|replaceAll('Hello, Hello world', 'Hello', 'Hi')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  replaceAll('Hello, Hello world', 'Hello', 'Hi')
