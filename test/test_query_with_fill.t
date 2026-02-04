ORDER BY WITH FILL FROM TO:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x FROM public.users ORDER BY users.x WITH FILL FROM 1 TO 10"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
  > let () = print_endline sql;;
  > ' --run-only
  >>> RUNNING
  SELECT users.x AS x
  FROM public.users AS users
  ORDER BY users.x ASC WITH FILL FROM 1 TO 10

ORDER BY WITH FILL STEP:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x FROM public.users ORDER BY users.x WITH FILL FROM 0 TO 100 STEP 5"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
  > let () = print_endline sql;;
  > ' --run-only
  >>> RUNNING
  SELECT users.x AS x
  FROM public.users AS users
  ORDER BY users.x ASC WITH FILL FROM 0 TO 100 STEP 5

ORDER BY WITH FILL INTERPOLATE:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x, users.id FROM public.users ORDER BY users.x WITH FILL FROM 0 TO 10 INTERPOLATE (id)"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
  > let () = print_endline sql;;
  > ' --run-only
  >>> RUNNING
  SELECT users.x AS x
  FROM public.users AS users
  ORDER BY users.x ASC WITH FILL FROM 0 TO 10 INTERPOLATE (id)

ORDER BY WITH FILL INTERPOLATE with AS expression:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x, users.id FROM public.users ORDER BY users.x WITH FILL FROM 0 TO 10 INTERPOLATE (id AS 0)"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
  > let () = print_endline sql;;
  > ' --run-only
  >>> RUNNING
  SELECT users.x AS x
  FROM public.users AS users
  ORDER BY users.x ASC WITH FILL FROM 0 TO 10 INTERPOLATE (id AS 0)

ORDER BY WITH FILL with params:
  $ ./compile_and_run '
  > let users ~start ~stop = [%q "SELECT users.x FROM public.users ORDER BY users.x WITH FILL FROM $.start TO $.stop"];;
  > #show users;;
  > ' --run-only
  >>> RUNNING
  val users :
    start:(< users : Ch_database.Public.users Ch_queries.scope > ->
           ('a, 'b) Ch_queries.expr) ->
    stop:(< users : Ch_database.Public.users Ch_queries.scope > ->
          ('c, 'd) Ch_queries.expr) ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

ORDER BY WITH FILL INTERPOLATE with param expression:
  $ ./compile_and_run '
  > let users ~default_id = [%q "SELECT users.x, users.id FROM public.users ORDER BY users.x WITH FILL FROM 0 TO 10 INTERPOLATE (id AS $.default_id)"];;
  > #show users;;
  > ' --run-only
  >>> RUNNING
  val users :
    default_id:(< users : Ch_database.Public.users Ch_queries.scope > ->
                ('a, 'b) Ch_queries.expr) ->
    < id : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr;
      x : (Ch_queries.non_null, string) Ch_queries.expr >
    Ch_queries.scope Ch_queries.select

ORDER BY DESC WITH FILL:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x FROM public.users ORDER BY users.x DESC WITH FILL FROM 100 TO 0 STEP -1"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
  > let () = print_endline sql;;
  > ' --run-only
  >>> RUNNING
  SELECT users.x AS x
  FROM public.users AS users
  ORDER BY users.x DESC WITH FILL FROM 100 TO 0 STEP negate(1)

Multiple ORDER BY - one with fill, one without:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x, users.id FROM public.users ORDER BY users.x WITH FILL FROM 1 TO 10, users.id DESC"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
  > let () = print_endline sql;;
  > ' --run-only
  >>> RUNNING
  SELECT users.x AS x
  FROM public.users AS users
  ORDER BY users.x ASC WITH FILL FROM 1 TO 10, users.id DESC

Multiple ORDER BY - both with different fills:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x, users.id FROM public.users ORDER BY users.x WITH FILL FROM 1 TO 10 STEP 1, users.id DESC WITH FILL FROM 100 TO 0 STEP -10"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
  > let () = print_endline sql;;
  > ' --run-only
  >>> RUNNING
  SELECT users.x AS x
  FROM public.users AS users
  ORDER BY users.x ASC WITH FILL FROM 1 TO 10 STEP 1, users.id DESC WITH FILL FROM 100 TO 0 STEP negate(10)
