  $ ./compile_and_run '
  > let sql, _ =
  >   let func x : {%t|Int32|} = Ch_queries.int x in
  >   let inner =
  >     {%q|SELECT users.id, $func AS func FROM public.users|} |> Ch_queries.from_select
  >   in
  >   let outer =
  >     let func {%s|users ..., ...|} x = {%e|users.func(${x})|} in
  >     {%q|SELECT users AS users, $.func AS func FROM $inner AS users|} |> Ch_queries.from_select
  >   in
  >   {%query_and_map|SELECT users.(users.id), users.func(${42}), users.func(${43}) FROM $outer AS users|};;
  > print_endline sql;;
  > ' --run-only
  >>> RUNNING
  SELECT users.id AS id, users.func AS _2, users.func_1 AS _3
  FROM (
    SELECT users.id AS id, users.func AS func, users.func_1 AS func_1
    FROM (
      SELECT users.id AS id, 42 AS func, 43 AS func_1 FROM public.users AS users)
      AS users) AS users
