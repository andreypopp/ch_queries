Testing toStringCutToZero:

  $ ./compile_and_run "
  > let e = {%e|toStringCutToZero('hello')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  toStringCutToZero('hello')
