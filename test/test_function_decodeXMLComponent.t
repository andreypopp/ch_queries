Testing decodeXMLComponent:

  $ ./compile_and_run "
  > let e = {%e|decodeXMLComponent('&lt;test&gt;')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  decodeXMLComponent('&lt;test&gt;')
