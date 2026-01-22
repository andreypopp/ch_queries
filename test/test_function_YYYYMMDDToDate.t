Testing YYYYMMDDToDate:

  $ ./compile_and_run "
  > let e = {%e|YYYYMMDDToDate(20250115)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, Ch_queries.date) Ch_queries.expr
  YYYYMMDDToDate(20250115)
