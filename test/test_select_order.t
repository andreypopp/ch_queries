querying columns:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x, users.is_active AS y FROM public.users"];;
  > let sql, parse_row = Ch_queries.query users @@ fun __q -> 
  >   let open Ch_queries in
  >   let open Ch_queries.Row in
  >   let+ x = Row.col [%e "q.x"] Parse.string
  >   and+ y = Row.col [%e "q.y"] Parse.bool
  >   in `List [`String x; `Bool y];;
  > let () = print_endline sql;;
  > let () = print_endline @@ Yojson.Basic.to_string @@ parse_row [`String "x"; `Bool false];;
  > ' --run-only
  >>> RUNNING
  SELECT q._1, q._2
  FROM (
    SELECT users.x AS _1, users.is_active AS _2 FROM public.users AS users) AS q
  ["x",false]

querying a list of columns:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x, users.is_active AS y FROM public.users"];;
  > let sql, parse_row = Ch_queries.query users @@ fun __q -> 
  >   let open Ch_queries in
  >   let open Ch_queries.Row in
  >   let+ json = Row.row [A_expr [%e "q.x"]; A_expr [%e "q.y"]] (fun json -> `List json)
  >   in json;;
  > let () = print_endline sql;;
  > let () = print_endline @@ Yojson.Basic.to_string @@ parse_row [`String "x"; `Bool false];;
  > ' --run-only
  >>> RUNNING
  SELECT q._2, q._1
  FROM (
    SELECT users.is_active AS _1, users.x AS _2 FROM public.users AS users) AS q
  ["x",false]
