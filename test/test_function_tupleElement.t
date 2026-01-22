Testing tupleElement:

  $ ./compile_and_run "
  > let e = {%e|tupleElement(tuple(1, 'hello'), 2)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : ('a, 'b) Ch_queries.expr
  tupleElement(tuple(1, 'hello'), 2)
