Testing arrayZip:

  $ ./compile_and_run "
  > let e = {%e|arrayZip([1, 2, 3], [4, 5, 6])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     (Ch_queries.non_null,
      ((Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr,
       (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr)
      Ch_queries.tuple2)
     Ch_queries.array)
    Ch_queries.expr
  arrayZip([1, 2, 3], [4, 5, 6])

Error when more than 2 arrays are provided:

  $ ./compile_and_run "
  > let e = {%e|arrayZip([1], [2], [3])|};;
  > "
  >>> PREPROCESSING
  File "-", line 2, characters 12-35:
  Error: arrayZip requires exactly 2 array arguments
  [1]
