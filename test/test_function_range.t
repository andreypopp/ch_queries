Testing range with one argument:

  $ ./compile_and_run "
  > let e = {%e|range(10)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, '_weak1 Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  range(10)

Testing range with two arguments:

  $ ./compile_and_run "
  > let e = {%e|range(5, 10)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, '_weak1 Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  range(5, 10)

Testing range with three arguments:

  $ ./compile_and_run "
  > let e = {%e|range(0, 10, 2)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, '_weak1 Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  range(0, 10, 2)
