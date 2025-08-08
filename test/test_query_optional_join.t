optional join with a table (elimination):
  $ ./compile_and_run '
  > let q = {%query|
  >   SELECT u.id AS user_id, p.name AS user_name
  >   FROM public.users AS u
  >   LEFT JOIN OPTIONAL public.profiles AS p
  >   ON u.id = p.user_id
  > |};;
  > let sql, _parse_row = Queries.(query q @@ fun q -> Row.int [%expr "q.user_id"]);;
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let q =
    Queries.select ()
      ~from:
        ((Queries.left_join ~optional:true)
           (Queries.from (Database.Public.users ~alias:"u" ~final:false))
           (Database.Public.profiles ~alias:"p" ~final:false)
           ~on:(fun ((u : _ Queries.scope), (p : _ Queries.scope)) ->
             Queries.Expr.( = )
               (u#query (fun u -> u#id))
               (p#query (fun p -> p#user_id))))
      ~select:(fun ((u : _ Queries.scope), (p : _ Queries.nullable_scope)) ->
        object
          method user_id = u#query (fun u -> u#id)
          method user_name = p#query (fun p -> p#name)
        end)
  
  let sql, _parse_row =
    let open Queries in
    query q @@ fun q -> Row.int (q#query (fun q -> q#user_id))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1 FROM (SELECT u.id AS _1 FROM public.users AS u) AS q

optional join with a table (in use):
  $ ./compile_and_run '
  > let q = {%query|
  >   SELECT u.id AS user_id, p.name AS user_name
  >   FROM public.users AS u
  >   LEFT JOIN OPTIONAL public.profiles AS p
  >   ON u.id = p.user_id
  > |};;
  > let sql, _parse_row = Queries.(query q @@ fun q -> Row.string_opt {%expr|q.user_name|});;
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let q =
    Queries.select ()
      ~from:
        ((Queries.left_join ~optional:true)
           (Queries.from (Database.Public.users ~alias:"u" ~final:false))
           (Database.Public.profiles ~alias:"p" ~final:false)
           ~on:(fun ((u : _ Queries.scope), (p : _ Queries.scope)) ->
             Queries.Expr.( = )
               (u#query (fun u -> u#id))
               (p#query (fun p -> p#user_id))))
      ~select:(fun ((u : _ Queries.scope), (p : _ Queries.nullable_scope)) ->
        object
          method user_id = u#query (fun u -> u#id)
          method user_name = p#query (fun p -> p#name)
        end)
  
  let sql, _parse_row =
    let open Queries in
    query q @@ fun q -> Row.string_opt (q#query (fun q -> q#user_name))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT p.name AS _1
    FROM public.users AS u LEFT JOIN public.profiles AS p ON u.id = p.user_id) AS
  q

optional join with a subquery (elimination):
  $ ./compile_and_run '
  > let q = {%query|
  >   SELECT u.id AS user_id, p.name AS user_name
  >   FROM public.users AS u
  >   LEFT JOIN OPTIONAL (SELECT p.user_id AS user_id, p.name AS name FROM public.profiles AS p) AS p
  >   ON u.id = p.user_id
  > |};;
  > let sql, _parse_row = Queries.(query q @@ fun q -> Row.int [%expr "q.user_id"]);;
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let q =
    Queries.select ()
      ~from:
        ((Queries.left_join ~optional:true)
           (Queries.from (Database.Public.users ~alias:"u" ~final:false))
           (Queries.from_select
              (Queries.select ()
                 ~from:
                   (Queries.from
                      (Database.Public.profiles ~alias:"p" ~final:false))
                 ~select:(fun (p : _ Queries.scope) ->
                   object
                     method user_id = p#query (fun p -> p#user_id)
                     method name = p#query (fun p -> p#name)
                   end))
              ~alias:"p")
           ~on:(fun ((u : _ Queries.scope), (p : _ Queries.scope)) ->
             Queries.Expr.( = )
               (u#query (fun u -> u#id))
               (p#query (fun p -> p#user_id))))
      ~select:(fun ((u : _ Queries.scope), (p : _ Queries.nullable_scope)) ->
        object
          method user_id = u#query (fun u -> u#id)
          method user_name = p#query (fun p -> p#name)
        end)
  
  let sql, _parse_row =
    let open Queries in
    query q @@ fun q -> Row.int (q#query (fun q -> q#user_id))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1 FROM (SELECT u.id AS _1 FROM public.users AS u) AS q

optional join with a subquery (in use):
  $ ./compile_and_run '
  > let q = {%query|
  >   SELECT u.id AS user_id, p.name AS user_name
  >   FROM public.users AS u
  >   LEFT JOIN OPTIONAL (SELECT p.user_id AS user_id, p.name AS name FROM public.profiles AS p) AS p
  >   ON u.id = p.user_id
  > |};;
  > let sql, _parse_row = Queries.(query q @@ fun q -> Row.string_opt [%expr "q.user_name"]);;
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let q =
    Queries.select ()
      ~from:
        ((Queries.left_join ~optional:true)
           (Queries.from (Database.Public.users ~alias:"u" ~final:false))
           (Queries.from_select
              (Queries.select ()
                 ~from:
                   (Queries.from
                      (Database.Public.profiles ~alias:"p" ~final:false))
                 ~select:(fun (p : _ Queries.scope) ->
                   object
                     method user_id = p#query (fun p -> p#user_id)
                     method name = p#query (fun p -> p#name)
                   end))
              ~alias:"p")
           ~on:(fun ((u : _ Queries.scope), (p : _ Queries.scope)) ->
             Queries.Expr.( = )
               (u#query (fun u -> u#id))
               (p#query (fun p -> p#user_id))))
      ~select:(fun ((u : _ Queries.scope), (p : _ Queries.nullable_scope)) ->
        object
          method user_id = u#query (fun u -> u#id)
          method user_name = p#query (fun p -> p#name)
        end)
  
  let sql, _parse_row =
    let open Queries in
    query q @@ fun q -> Row.string_opt (q#query (fun q -> q#user_name))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT p._1 AS _1
    FROM public.users AS u
    LEFT JOIN
    (SELECT p.name AS _1, p.user_id AS _2 FROM public.profiles AS p) AS p
    ON
    u.id = p._2) AS q
