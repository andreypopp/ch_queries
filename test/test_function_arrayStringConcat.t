Testing arrayStringConcat:

  $ ./compile_and_run "
  > let e = {%e|arrayStringConcat(['a', 'b', 'c'])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  arrayStringConcat(['a', 'b', 'c'])

Testing arrayStringConcat with separator:

  $ ./compile_and_run "
  > let e = {%e|arrayStringConcat(['a', 'b', 'c'], ',')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  arrayStringConcat(['a', 'b', 'c'], ',')
