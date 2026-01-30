Testing substring with 2 arguments:

  $ ./compile_and_run "
  > let e = {%e|substring('hello world', 7)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  substring('hello world', 7)

Testing substring with 3 arguments:

  $ ./compile_and_run "
  > let e = {%e|substring('hello world', 7, 5)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  substring('hello world', 7, 5)

Testing substringUTF8 with 2 arguments:

  $ ./compile_and_run "
  > let e = {%e|substringUTF8('hello world', 7)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  substringUTF8('hello world', 7)

Testing substringUTF8 with 3 arguments:

  $ ./compile_and_run "
  > let e = {%e|substringUTF8('hello world', 7, 5)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  substringUTF8('hello world', 7, 5)
