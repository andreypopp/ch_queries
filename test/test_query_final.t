select from table with FINAL keyword:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x FROM public.users AS users FINAL WHERE users.is_active"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun users -> Ch_queries.Row.string [%e "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.from (Ch_database.Public.users ~alias:"users" ~final:true))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~where:(fun (users : _ Ch_queries.scope) ->
        users#query (fun users -> users#is_active))
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun users ->
    Ch_queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users.x AS _1 FROM public.users AS users FINAL WHERE users.is_active)
    AS q

if FINAL keyword is applied to param, then it expects the table:
  $ ./compile_and_run '
  > let users table = [%q "SELECT users.x AS x FROM ?table AS users FINAL WHERE users.is_active"];;
  > let users = users Ch_database.Public.users;;
  > let sql, _parse_row = Ch_queries.query users @@ fun users -> Ch_queries.Row.string [%e "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users table =
    Ch_queries.select ()
      ~from:(Ch_queries.from (table ~final:true ~alias:"users"))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~where:(fun (users : _ Ch_queries.scope) ->
        users#query (fun users -> users#is_active))
  
  let users = users Ch_database.Public.users
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun users ->
    Ch_queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users.x AS _1 FROM public.users AS users FINAL WHERE users.is_active)
    AS q
