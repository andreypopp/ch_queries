Testing arrayEnumerateUniq:

  $ ./compile_and_run "
  > let e = {%e|arrayEnumerateUniq(['a', 'b', 'a', 'c', 'b'])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, int Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  arrayEnumerateUniq(['a', 'b', 'a', 'c', 'b'])

Testing arrayEnumerateUniq with multiple arrays:

  $ ./compile_and_run "
  > let e = {%e|arrayEnumerateUniq(['a', 'b', 'a', 'c', 'b'], ['x', 'y', 'x', 'z', 'y'])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, int Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  arrayEnumerateUniq(['a', 'b', 'a', 'c', 'b'], ['x', 'y', 'x', 'z', 'y'])
