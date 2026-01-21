Testing arrayPartialShuffle:

  $ ./compile_and_run "
  > let e = {%e|arrayPartialShuffle([1, 2, 3])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, int Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  arrayPartialShuffle([1, 2, 3])

Testing arrayPartialShuffle with limit:

  $ ./compile_and_run "
  > let e = {%e|arrayPartialShuffle([1, 2, 3], 2)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, int Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  arrayPartialShuffle([1, 2, 3], 2)

Testing arrayPartialShuffle with limit and seed:

  $ ./compile_and_run "
  > let e = {%e|arrayPartialShuffle([1, 2, 3], 2, 42)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null, int Ch_queries.number) Ch_queries.array)
    Ch_queries.expr
  arrayPartialShuffle([1, 2, 3], 2, 42)
