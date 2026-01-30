Testing splitByChar:

  $ ./compile_and_run "
  > let e = {%e|splitByChar(',', 'a,b,c')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null, (Ch_queries.non_null, string) Ch_queries.array)
    Ch_queries.expr
  splitByChar(',', 'a,b,c')

Testing splitByChar with max_substrings:

  $ ./compile_and_run "
  > let e = {%e|splitByChar(',', 'a,b,c,d', 2)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null, (Ch_queries.non_null, string) Ch_queries.array)
    Ch_queries.expr
  splitByChar(',', 'a,b,c,d', 2)
