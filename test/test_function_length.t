Testing length on arrays:

  $ ./compile_and_run "
  > let e = {%e|length([1, 2, 3])|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  length([1, 2, 3])

Testing length on string literals:

  $ ./compile_and_run "
  > let e = {%e|length('hello')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  length('hello')

Testing length on strings (via type ascription):

  $ ./compile_and_run "
  > let p = {%e|'hello'|};;
  > let e = {%e|length(\$p::String)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  length('hello')
