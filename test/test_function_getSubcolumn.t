Testing getSubcolumn:

  $ ./compile_and_run "
  > let e = {%e|getSubcolumn(1, 'null')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : ('a, 'b) Ch_queries.expr
  getSubcolumn(1, 'null')
