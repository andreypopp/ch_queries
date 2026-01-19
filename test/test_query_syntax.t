select:
  $ ./compile_and_run '
  > let q = [%query_syntax "SELECT q.x AS x, q.is_active FROM (SELECT users.x AS x, users.is_active AS is_active FROM public.users) AS q WHERE q.is_active"];;
  > let sql = Ch_queries_syntax.Printer.print_query q;;
  > let () = print_endline sql;;
  > ' --run-only
  >>> RUNNING
  SELECT q._2 AS x, q._1 AS is_active
  FROM (
    SELECT users.is_active AS _1, users.x AS _2 FROM public.users AS users) AS q
  WHERE q._1
