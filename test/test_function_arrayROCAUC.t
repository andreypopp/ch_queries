Testing arrayROCAUC:

  $ ./compile_and_run "
  > let e = {%e|arrayROCAUC([0.1, 0.4, 0.35, 0.8], [0, 0, 1, 1])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, float Ch_queries.number) Ch_queries.expr
  arrayROCAUC([0.1, 0.4, 0.35, 0.8], [0, 0, 1, 1])

Testing arrayROCAUC with scale:

  $ ./compile_and_run "
  > let e = {%e|arrayROCAUC([0.1, 0.4, 0.35, 0.8], [0, 0, 1, 1], false)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, float Ch_queries.number) Ch_queries.expr
  arrayROCAUC([0.1, 0.4, 0.35, 0.8], [0, 0, 1, 1], false)

Testing arrayROCAUC with scale and partial_offsets:

  $ ./compile_and_run "
  > let e = {%e|arrayROCAUC([0.1, 0.4], [0, 1], true, [10, 5, 100, 50])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, float Ch_queries.number) Ch_queries.expr
  arrayROCAUC([0.1, 0.4], [0, 1], true, [10, 5, 100, 50])
