Programmatic union
  $ ./compile_and_run '
  > let users1 = [%query "SELECT 1 AS x FROM public.users"];;
  > let users2 = [%query "SELECT 2 AS x FROM public.users"];;
  > let users = Queries.union users1 users2;;
  > let sql, _parse_row = Queries.query users @@ fun users -> Queries.Row.int [%expr "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users1 =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method x = Queries.int 1
        end)
  
  let users2 =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method x = Queries.int 2
        end)
  
  let users = Queries.union users1 users2
  
  let sql, _parse_row =
    Queries.query users @@ fun users ->
    Queries.Row.int (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT 1 AS _1 FROM public.users AS users
    UNION
    SELECT 2 AS _1 FROM public.users AS users) AS q

UNION syntax:
  $ ./compile_and_run '
  > let users = [%query "SELECT 1 AS x FROM public.users UNION SELECT 2 AS x FROM public.users"];;
  > let sql, _parse_row = Queries.query users @@ fun users -> Queries.Row.int [%expr "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Queries.union
      (Queries.select ()
         ~from:(Queries.from (Database.Public.users ~alias:"users" ~final:false))
         ~select:(fun (users : _ Queries.scope) ->
           object
             method x = Queries.int 1
           end))
      (Queries.select ()
         ~from:(Queries.from (Database.Public.users ~alias:"users" ~final:false))
         ~select:(fun (users : _ Queries.scope) ->
           object
             method x = Queries.int 2
           end))
  
  let sql, _parse_row =
    Queries.query users @@ fun users ->
    Queries.Row.int (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT 1 AS _1 FROM public.users AS users
    UNION
    SELECT 2 AS _1 FROM public.users AS users) AS q
