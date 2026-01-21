Testing arrayLevenshteinDistanceWeighted:

  $ ./compile_and_run "
  > let e = {%e|arrayLevenshteinDistanceWeighted([1, 2, 3], [2, 3, 4], [1.0, 1.0, 1.0], [1.0, 1.0, 1.0])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, float Ch_queries.number) Ch_queries.expr
  arrayLevenshteinDistanceWeighted([1, 2, 3], [2, 3, 4], [1., 1., 1.], [1., 1., 1.])
