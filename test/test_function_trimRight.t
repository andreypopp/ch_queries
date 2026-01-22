Testing trimRight without trim_characters:

  $ ./compile_and_run "
  > let e = {%e|trimRight('hello  ')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  trimRight('hello  ')

Testing trimRight with trim_characters:

  $ ./compile_and_run "
  > let e = {%e|trimRight('helloxx', 'x')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  trimRight('helloxx', 'x')
