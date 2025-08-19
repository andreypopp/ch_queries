Programmatic union
  $ ./compile_and_run '
  > let users1 = [%q "SELECT 1 AS x FROM public.users"];;
  > let users2 = [%q "SELECT 2 AS x FROM public.users"];;
  > let users = [%q "?users1 UNION ?users2"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.int [%e "q.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users1 =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from
              (Ch_database.Public.users ~alias:"users" ~final:false))
           (fun (users : _ Ch_queries.scope) ->
             object
               method users = users
             end))
      ~select:(fun __q ->
        object
          method x = Ch_queries.int 1
        end)
  
  let users2 =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from
              (Ch_database.Public.users ~alias:"users" ~final:false))
           (fun (users : _ Ch_queries.scope) ->
             object
               method users = users
             end))
      ~select:(fun __q ->
        object
          method x = Ch_queries.int 2
        end)
  
  let users = Ch_queries.union users1 users2
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.int (__q#q#query (fun q -> q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT 1 AS _1 FROM public.users AS users
    UNION
    SELECT 2 AS _1 FROM public.users AS users) AS q

UNION syntax:
  $ ./compile_and_run '
  > let users = [%q "SELECT 1 AS x FROM public.users UNION SELECT 2 AS x FROM public.users"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.int [%e "q.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.union
      (Ch_queries.select ()
         ~from:
           (Ch_queries.map_from_scope
              (Ch_queries.from
                 (Ch_database.Public.users ~alias:"users" ~final:false))
              (fun (users : _ Ch_queries.scope) ->
                object
                  method users = users
                end))
         ~select:(fun __q ->
           object
             method x = Ch_queries.int 1
           end))
      (Ch_queries.select ()
         ~from:
           (Ch_queries.map_from_scope
              (Ch_queries.from
                 (Ch_database.Public.users ~alias:"users" ~final:false))
              (fun (users : _ Ch_queries.scope) ->
                object
                  method users = users
                end))
         ~select:(fun __q ->
           object
             method x = Ch_queries.int 2
           end))
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.int (__q#q#query (fun q -> q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT 1 AS _1 FROM public.users AS users
    UNION
    SELECT 2 AS _1 FROM public.users AS users) AS q
