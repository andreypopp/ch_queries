Testing lagInFrame:

  $ ./compile_and_run "
  > let e = {%e|lagInFrame(1) over(partition by 1 order by 2)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  lagInFrame(1) OVER (PARTITION BY 1 ORDER BY 2 ASC)

Testing lagInFrame with offset:

  $ ./compile_and_run "
  > let e = {%e|lagInFrame(1, 2) over(partition by 1 order by 2)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  lagInFrame(1, 2) OVER (PARTITION BY 1 ORDER BY 2 ASC)

Testing lagInFrame with offset and default:

  $ ./compile_and_run "
  > let e = {%e|lagInFrame(1, 2, 0) over(partition by 1 order by 2)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  lagInFrame(1, 2, 0) OVER (PARTITION BY 1 ORDER BY 2 ASC)
