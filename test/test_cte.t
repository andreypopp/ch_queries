CTE is supported:
  $ ./compile_and_run '
  > let q = {%q|
  >   WITH users AS (SELECT users.x AS x FROM public.users)
  >   SELECT x.x AS a, y.x AS b FROM users AS x JOIN users AS y ON x.x = y.x
  > |};;
  > let sql, _parse_row = Ch_queries.query q @@ fun __q -> Ch_queries.Row.ignore [%e "[q.a, q.b]"];;
  > print_endline sql;;
  > '
  >>> PREPROCESSING
  let q =
    let users =
      Ch_queries.from_select ~cte:{ materialized = false }
        (Ch_queries.select ()
           ~from:
             (Ch_queries.map_from_scope
                (Ch_queries.from
                   (Ch_database.Public.users ~alias:"users" ~final:false))
                (fun (users : _ Ch_queries.scope) ->
                  let __q =
                    object
                      method users = users
                    end
                  in
                  object
                    method x = __q#users#query (fun __q -> __q#x)
                    method users = users
                  end))
           ~select:(fun __q ->
             object
               method x = __q#x
             end))
    in
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.join
              (Ch_queries.from (users ~alias:"x"))
              (users ~alias:"y")
              ~on:(fun ((x : _ Ch_queries.scope), (y : _ Ch_queries.scope)) ->
                let __q =
                  object
                    method x = x
                    method y = y
                  end
                in
                Ch_queries.Expr.( = )
                  (__q#x#query (fun __q -> __q#x))
                  (__q#y#query (fun __q -> __q#x))))
           (fun ((x : _ Ch_queries.scope), (y : _ Ch_queries.scope)) ->
             let __q =
               object
                 method x = x
                 method y = y
               end
             in
             object
               method a = __q#x#query (fun __q -> __q#x)
               method b = __q#y#query (fun __q -> __q#x)
               method x = x
               method y = y
             end))
      ~select:(fun __q ->
        object
          method a = __q#a
          method b = __q#b
        end)
  
  let sql, _parse_row =
    Ch_queries.query q @@ fun __q ->
    Ch_queries.Row.ignore
      (Ch_queries.array
         [ __q#q#query (fun __q -> __q#a); __q#q#query (fun __q -> __q#b) ])
  ;;
  
  print_endline sql
  >>> RUNNING
  SELECT [q._2, q._1]
  FROM (
    WITH x AS (SELECT users.x AS _1 FROM public.users AS users)
    SELECT y._1 AS _1, x._1 AS _2
    FROM x AS x INNER JOIN x AS y ON x._1 = y._1) AS q

CTE can be also defined programmatically and then used as query params:
  $ ./compile_and_run '
  > let users = Ch_queries.from_select ~cte:{materialized=false} [%q "SELECT users.x AS x FROM public.users"];;
  > let q = {%q|SELECT x.x AS a, y.x AS b FROM $users AS x JOIN $users AS y ON x.x = y.x|};;
  > let sql, _parse_row = Ch_queries.query q @@ fun __q -> Ch_queries.Row.ignore [%e "[q.a, q.b]"];;
  > print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.from_select ~cte:{ materialized = false }
      (Ch_queries.select ()
         ~from:
           (Ch_queries.map_from_scope
              (Ch_queries.from
                 (Ch_database.Public.users ~alias:"users" ~final:false))
              (fun (users : _ Ch_queries.scope) ->
                let __q =
                  object
                    method users = users
                  end
                in
                object
                  method x = __q#users#query (fun __q -> __q#x)
                  method users = users
                end))
         ~select:(fun __q ->
           object
             method x = __q#x
           end))
  
  let q =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.join
              (Ch_queries.from (users ~alias:"x"))
              (users ~alias:"y")
              ~on:(fun ((x : _ Ch_queries.scope), (y : _ Ch_queries.scope)) ->
                let __q =
                  object
                    method x = x
                    method y = y
                  end
                in
                Ch_queries.Expr.( = )
                  (__q#x#query (fun __q -> __q#x))
                  (__q#y#query (fun __q -> __q#x))))
           (fun ((x : _ Ch_queries.scope), (y : _ Ch_queries.scope)) ->
             let __q =
               object
                 method x = x
                 method y = y
               end
             in
             object
               method a = __q#x#query (fun __q -> __q#x)
               method b = __q#y#query (fun __q -> __q#x)
               method x = x
               method y = y
             end))
      ~select:(fun __q ->
        object
          method a = __q#a
          method b = __q#b
        end)
  
  let sql, _parse_row =
    Ch_queries.query q @@ fun __q ->
    Ch_queries.Row.ignore
      (Ch_queries.array
         [ __q#q#query (fun __q -> __q#a); __q#q#query (fun __q -> __q#b) ])
  ;;
  
  print_endline sql
  >>> RUNNING
  SELECT [q._2, q._1]
  FROM (
    WITH x AS (SELECT users.x AS _1 FROM public.users AS users)
    SELECT y._1 AS _1, x._1 AS _2
    FROM x AS x INNER JOIN x AS y ON x._1 = y._1) AS q

materilized CTE is supported as well:
  $ ./compile_and_run '
  > let q = {%q|
  >   WITH users AS MATERIALIZED (SELECT users.x AS x FROM public.users)
  >   SELECT x.x AS a, y.x AS b FROM users AS x JOIN users AS y ON x.x = y.x
  > |};;
  > let sql, _parse_row = Ch_queries.query q @@ fun __q -> Ch_queries.Row.ignore [%e "[q.a, q.b]"];;
  > print_endline sql;;
  > '
  >>> PREPROCESSING
  let q =
    let users =
      Ch_queries.from_select ~cte:{ materialized = true }
        (Ch_queries.select ()
           ~from:
             (Ch_queries.map_from_scope
                (Ch_queries.from
                   (Ch_database.Public.users ~alias:"users" ~final:false))
                (fun (users : _ Ch_queries.scope) ->
                  let __q =
                    object
                      method users = users
                    end
                  in
                  object
                    method x = __q#users#query (fun __q -> __q#x)
                    method users = users
                  end))
           ~select:(fun __q ->
             object
               method x = __q#x
             end))
    in
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.join
              (Ch_queries.from (users ~alias:"x"))
              (users ~alias:"y")
              ~on:(fun ((x : _ Ch_queries.scope), (y : _ Ch_queries.scope)) ->
                let __q =
                  object
                    method x = x
                    method y = y
                  end
                in
                Ch_queries.Expr.( = )
                  (__q#x#query (fun __q -> __q#x))
                  (__q#y#query (fun __q -> __q#x))))
           (fun ((x : _ Ch_queries.scope), (y : _ Ch_queries.scope)) ->
             let __q =
               object
                 method x = x
                 method y = y
               end
             in
             object
               method a = __q#x#query (fun __q -> __q#x)
               method b = __q#y#query (fun __q -> __q#x)
               method x = x
               method y = y
             end))
      ~select:(fun __q ->
        object
          method a = __q#a
          method b = __q#b
        end)
  
  let sql, _parse_row =
    Ch_queries.query q @@ fun __q ->
    Ch_queries.Row.ignore
      (Ch_queries.array
         [ __q#q#query (fun __q -> __q#a); __q#q#query (fun __q -> __q#b) ])
  ;;
  
  print_endline sql
  >>> RUNNING
  SELECT [q._2, q._1]
  FROM (
    WITH x AS MATERIALIZED (SELECT users.x AS _1 FROM public.users AS users)
    SELECT y._1 AS _1, x._1 AS _2
    FROM x AS x INNER JOIN x AS y ON x._1 = y._1) AS q
