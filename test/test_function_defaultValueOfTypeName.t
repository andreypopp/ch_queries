Testing defaultValueOfTypeName:

  $ ./compile_and_run "
  > let e = {%e|defaultValueOfTypeName('Int8')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : ('a, 'b) Ch_queries.expr
  defaultValueOfTypeName('Int8')
