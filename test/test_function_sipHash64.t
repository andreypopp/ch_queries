Testing sipHash64 with single argument:

  $ ./compile_and_run "
  > let e = {%e|sipHash64('hello')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null, Ch_queries.uint64 Ch_queries.number) Ch_queries.expr
  sipHash64('hello')

Testing sipHash64 with multiple arguments:

  $ ./compile_and_run "
  > let e = {%e|sipHash64('hello', 'world')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null, Ch_queries.uint64 Ch_queries.number) Ch_queries.expr
  sipHash64('hello', 'world')

