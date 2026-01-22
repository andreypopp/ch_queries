Testing age:

  $ ./compile_and_run "
  > let e = {%e|age('hour', toDateTime('2018-01-01 22:30:00'), toDateTime('2018-01-02 23:00:00'))|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  age('hour', toDateTime('2018-01-01 22:30:00'), toDateTime('2018-01-02 23:00:00'))

Testing age with timezone:

  $ ./compile_and_run "
  > let e = {%e|age('hour', toDateTime('2018-01-01 22:30:00'), toDateTime('2018-01-02 23:00:00'), 'UTC')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  age('hour', toDateTime('2018-01-01 22:30:00'), toDateTime('2018-01-02 23:00:00'), 'UTC')
