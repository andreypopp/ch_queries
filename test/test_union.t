Programmatic union
  $ ./compile_and_run '
  > let users1 = [%q "SELECT 1 AS x FROM public.users"];;
  > let users2 = [%q "SELECT 2 AS x FROM public.users"];;
  > let users = [%q $users1 UNION $users2"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.int [%e "q.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  File "-", line 4, characters 16-17:
  4 | let users = [%q $users1 UNION $users2"];;
                      ^
  Error: Syntax error
  >>> RUNNING
  File "./test_query.ml", line 10, characters 16-17:
  10 | let users = [%q $users1 UNION $users2"];;
                       ^
  Error: Syntax error
  [2]

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
    Ch_queries.Row.int (__q#q#query (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT 1 AS _1 FROM public.users AS users
    UNION
    SELECT 2 AS _1 FROM public.users AS users) AS q
