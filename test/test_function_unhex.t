Testing unhex:

  $ ./compile_and_run "
  > let e = {%e|unhex('48656C6C6F')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  unhex('48656C6C6F')
