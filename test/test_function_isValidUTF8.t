Testing isValidUTF8:

  $ ./compile_and_run "
  > let e = {%e|isValidUTF8('hello')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, bool) Ch_queries.expr
  isValidUTF8('hello')
