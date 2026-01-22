Testing IPv4StringToNumOrDefault:

  $ ./compile_and_run "
  > let e = {%e|iPv4StringToNumOrDefault('192.168.0.1', 0)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  IPv4StringToNumOrDefault('192.168.0.1', 0)
