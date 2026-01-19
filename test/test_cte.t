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
    Ch_queries.with_cte ~materialized:false ~alias:"users"
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
                  method users = users
                  method x = __q#users#query (fun __q -> __q#x)
                end))
         ~select:(fun __q ->
           object
             method x = __q#x
           end))
      (fun users ->
        Ch_queries.select ()
          ~from:
            (Ch_queries.map_from_scope
               (Ch_queries.join
                  (Ch_queries.from
                     ((users : alias:string -> _ Ch_queries.from_one) ~alias:"x"))
                  ((users : alias:string -> _ Ch_queries.from_one) ~alias:"y")
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
                   method x = x
                   method y = y
                   method a = __q#x#query (fun __q -> __q#x)
                   method b = __q#y#query (fun __q -> __q#x)
                 end))
          ~select:(fun __q ->
            object
              method a = __q#a
              method b = __q#b
            end))
  
  let sql, _parse_row =
    Ch_queries.query q @@ fun __q ->
    Ch_queries.Row.ignore
      (Ch_queries.array
         [ __q#q#query (fun __q -> __q#a); __q#q#query (fun __q -> __q#b) ])
  ;;
  
  print_endline sql
  >>> RUNNING
  SELECT [q.x_1, q.x]
  FROM (
    WITH users AS (SELECT users.x AS x FROM public.users AS users)
    SELECT y.x AS x, x.x AS x_1
    FROM users AS x INNER JOIN users AS y ON x.x = y.x) AS q

CTE can be defined programmatically and then used as query params:
  $ ./compile_and_run '
  > let q = Ch_queries.with_cte ~alias:"users"
  >   {%q|SELECT users.x AS x FROM public.users|}
  >   (fun users -> {%q|SELECT x.x AS a, y.x AS b FROM users AS x JOIN users AS y ON x.x = y.x|});;
  > let sql, _parse_row = Ch_queries.query q @@ fun __q -> Ch_queries.Row.ignore [%e "[q.a, q.b]"];;
  > print_endline sql;;
  > ' --run-only
  >>> RUNNING
  SELECT [q.x_1, q.x]
  FROM (
    WITH users AS (SELECT users.x AS x FROM public.users AS users)
    SELECT y.x AS x, x.x AS x_1
    FROM users AS x INNER JOIN users AS y ON x.x = y.x) AS q

materilized CTE is supported as well:
  $ ./compile_and_run '
  > let q = {%q|
  >   WITH users AS MATERIALIZED (SELECT users.x AS x FROM public.users)
  >   SELECT x.x AS a, y.x AS b FROM users AS x JOIN users AS y ON x.x = y.x
  > |};;
  > let sql, _parse_row = Ch_queries.query q @@ fun __q -> Ch_queries.Row.ignore [%e "[q.a, q.b]"];;
  > print_endline sql;;
  > ' --run-only
  >>> RUNNING
  SELECT [q.x_1, q.x]
  FROM (
    WITH users AS MATERIALIZED (SELECT users.x AS x FROM public.users AS users)
    SELECT y.x AS x, x.x AS x_1
    FROM users AS x INNER JOIN users AS y ON x.x = y.x) AS q

a query within CTE can be a param:
  $ ./compile_and_run '
  > let s = {%q|SELECT users.x AS x FROM public.users|};;
  > let q = {%q|WITH u AS ($s) SELECT u.x AS a FROM u|};;
  > let sql, _parse_row = Ch_queries.query q @@ fun __q -> Ch_queries.Row.ignore [%e "q.a"];;
  > print_endline sql;;
  > ' --run-only
  >>> RUNNING
  WITH u AS (SELECT users.x AS x FROM public.users AS users)
  SELECT u.x AS x
  FROM u AS u

a CTE used in IN:
  $ ./compile_and_run '
  > let q = {%q|
  > WITH u AS (SELECT users.x AS x, users.is_active AS is_active FROM public.users)
  > SELECT 1 AS a FROM u
  > WHERE u.x IN (SELECT u.x AS _1 FROM u) AND u.is_active
  > |};;
  > let sql, _parse_row = Ch_queries.query q @@ fun __q -> Ch_queries.Row.ignore [%e "q.a"];;
  > print_endline sql;;
  > ' --run-only
  >>> RUNNING
  WITH
    u AS (
      SELECT users.is_active AS is_active, users.x AS x FROM public.users AS users
    )
  SELECT 1 AS _1
  FROM u AS u
  WHERE u.x IN (SELECT u.x AS x FROM u AS u) AND u.is_active

a CTE used in IN (only IN references CTE):
  $ ./compile_and_run '
  > let q = {%q|
  > WITH u AS (SELECT users.x AS x FROM public.users)
  > SELECT 1 AS a FROM u
  > WHERE u.x IN (SELECT u.x AS _1 FROM u)
  > |};;
  > let sql, _parse_row = Ch_queries.query q @@ fun __q -> Ch_queries.Row.ignore [%e "q.a"];;
  > print_endline sql;;
  > ' --run-only
  >>> RUNNING
  WITH u AS (SELECT users.x AS x FROM public.users AS users)
  SELECT 1 AS _1
  FROM u AS u
  WHERE u.x IN (SELECT u.x AS x FROM u AS u)
