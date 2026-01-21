Testing indexOfAssumeSorted:

  $ ./compile_and_run "
  > let e = {%e|indexOfAssumeSorted([1, 2, 3, 4, 5], 3)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int64 Ch_queries.number) Ch_queries.expr
  indexOfAssumeSorted([1, 2, 3, 4, 5], 3)
