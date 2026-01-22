Testing IPv4NumToString:

  $ ./compile_and_run "
  > let e = {%e|iPv4NumToString(3232235521)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  IPv4NumToString(3232235521)
