Testing arrayZip:

  $ ./compile_and_run "
  > let e = {%e|arrayZip([1, 2, 3], [4, 5, 6])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null, ('_weak1, '_weak2) Ch_queries.array) Ch_queries.expr
  arrayZip([1, 2, 3], [4, 5, 6])
