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
  SELECT users.x AS x, users.is_active AS is_active FROM public.users AS users
  ["x",false]

querying a list of columns (while `y` evals before `x` the order is still intact):
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x, users.is_active AS y FROM public.users"];;
  > let sql, parse_row = Ch_queries.query users @@ fun __q -> 
  >   let open Ch_queries in
  >   let open Ch_queries.Row in
  >   let y = A_expr [%e "q.y"] in
  >   let x = A_expr [%e "q.x"] in
  >   let+ json = Row.row [x; y] (fun json -> `List json)
  >   in json;;
  > let () = print_endline sql;;
  > let () = print_endline @@ Yojson.Basic.to_string @@ parse_row [`String "x"; `Bool false];;
  > ' --run-only
  >>> RUNNING
  SELECT users.x AS x, users.is_active AS is_active FROM public.users AS users
  ["x",false]

querying columns, same column twice:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x, users.is_active AS y FROM public.users"];;
  > let sql, parse_row = Ch_queries.query users @@ fun __q -> 
  >   let open Ch_queries in
  >   let open Ch_queries.Row in
  >   let+ x = Row.col [%e "q.x"] Parse.string
  >   and+ y = Row.col [%e "q.x"] Parse.string
  >   in `List [`String x; `String y];;
  > let () = print_endline sql;;
  > let () = print_endline @@ Yojson.Basic.to_string @@ parse_row [`String "x"; `String "y"];;
  > ' --run-only
  >>> RUNNING
  SELECT users.x AS x, users.x AS x FROM public.users AS users
  ["x","y"]
