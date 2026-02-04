Testing accurateCastOrNull:

  $ ./compile_and_run "
  > let e = {%e|accurateCastOrNull(-1, 'UInt8')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.null, 'a) Ch_queries.expr
  accurateCastOrNull(negate(1), 'UInt8')
