Testing right:

  $ ./compile_and_run "
  > let e = {%e|right('hello', 3)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  right('hello', 3)
