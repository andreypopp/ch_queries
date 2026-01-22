Testing trimBoth without trim_characters:

  $ ./compile_and_run "
  > let e = {%e|trimBoth('  hello  ')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  trimBoth('  hello  ')

Testing trimBoth with trim_characters:

  $ ./compile_and_run "
  > let e = {%e|trimBoth('xxhelloxx', 'x')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  trimBoth('xxhelloxx', 'x')
