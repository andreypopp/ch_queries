Testing toIPv4:

  $ ./compile_and_run "
  > let e = {%e|toIPv4('192.168.0.1')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  toIPv4('192.168.0.1')
