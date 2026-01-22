Testing addNanoseconds:

  $ ./compile_and_run "
  > let e = {%e|addNanoseconds(now(), 1000)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, Ch_queries.datetime64) Ch_queries.expr
  addNanoseconds(now(), 1000)
