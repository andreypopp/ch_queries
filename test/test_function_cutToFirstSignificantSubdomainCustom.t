Testing cutToFirstSignificantSubdomainCustom:

  $ ./compile_and_run "
  > let e = {%e|cutToFirstSignificantSubdomainCustom('www.example.co.uk', 'public_suffix_list')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e : (Ch_queries.non_null, string) Ch_queries.expr
  cutToFirstSignificantSubdomainCustom('www.example.co.uk', 'public_suffix_list')
