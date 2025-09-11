can define expression for reuse with `WITH` clause:
  $ ./compile_and_run '
  > let users = {%q|WITH 1 AS x SELECT u.id, x + u.id AS id_and_x FROM public.users AS u|};;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.id_and_x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from (Ch_database.Public.users ~alias:"u" ~final:false))
           (fun (u : _ Ch_queries.scope) ->
             let __q =
               object
                 method u = u
               end
             in
             let __q =
               object
                 method u = u
                 method x = Ch_queries.int 1
               end
             in
             object
               method u = u
               method x = Ch_queries.int 1
  
               method id_and_x =
                 Ch_queries.Expr.( + ) __q#x (__q#u#query (fun __q -> __q#id))
             end))
      ~select:(fun __q ->
        object
          method id = __q#u#query (fun __q -> __q#id)
          method id_and_x = __q#id_and_x
        end)
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query (fun __q -> __q#id_and_x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1 FROM (SELECT 1 + u.id AS _1 FROM public.users AS u) AS q

select shadows `WITH` clause:
  $ ./compile_and_run '
  > let users = {%q|WITH 1 AS x SELECT x + u.id AS x FROM public.users AS u|};;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from (Ch_database.Public.users ~alias:"u" ~final:false))
           (fun (u : _ Ch_queries.scope) ->
             let __q =
               object
                 method u = u
               end
             in
             let __q =
               object
                 method u = u
                 method x = Ch_queries.int 1
               end
             in
             object
               method u = u
  
               method x =
                 Ch_queries.Expr.( + ) __q#x (__q#u#query (fun __q -> __q#id))
             end))
      ~select:(fun __q ->
        object
          method x = __q#x
        end)
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1 FROM (SELECT 1 + u.id AS _1 FROM public.users AS u) AS q

WITH has access to table columns:
  $ ./compile_and_run '
  > let users = {%q|WITH u.id AS x SELECT x + u.id AS y FROM public.users AS u|};;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.y"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from (Ch_database.Public.users ~alias:"u" ~final:false))
           (fun (u : _ Ch_queries.scope) ->
             let __q =
               object
                 method u = u
               end
             in
             let __q =
               object
                 method u = u
                 method x = __q#u#query (fun __q -> __q#id)
               end
             in
             object
               method u = u
               method x = __q#u#query (fun __q -> __q#id)
  
               method y =
                 Ch_queries.Expr.( + ) __q#x (__q#u#query (fun __q -> __q#id))
             end))
      ~select:(fun __q ->
        object
          method y = __q#y
        end)
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query (fun __q -> __q#y))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1 FROM (SELECT u.id + u.id AS _1 FROM public.users AS u) AS q
