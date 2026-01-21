Testing arrayLevenshteinDistance:

  $ ./compile_and_run "
  > let e = {%e|arrayLevenshteinDistance([1, 2, 3], [2, 3, 4])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, float Ch_queries.number) Ch_queries.expr
  arrayLevenshteinDistance([1, 2, 3], [2, 3, 4])
