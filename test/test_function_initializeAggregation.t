Testing initializeAggregation:

  $ ./compile_and_run "
  > let e = {%e|initializeAggregation('sumState', 1)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : ('a, 'b) Ch_queries.expr
  initializeAggregation('sumState', 1)
