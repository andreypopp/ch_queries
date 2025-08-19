optional join with a table (elimination):
  $ ./compile_and_run '
  > let q = {%q|
  >   SELECT u.id AS user_id, p.name AS user_name
  >   FROM public.users AS u
  >   LEFT JOIN OPTIONAL public.profiles AS p
  >   ON u.id = p.user_id
  > |};;
  > let sql, _parse_row = Ch_queries.(query q @@ fun __q -> Row.int [%e "q.user_id"]);;
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let q =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           ((Ch_queries.left_join ~optional:true)
              (Ch_queries.from (Ch_database.Public.users ~alias:"u" ~final:false))
              (Ch_database.Public.profiles ~alias:"p" ~final:false)
              ~on:(fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.scope)) ->
                let __q =
                  object
                    method u = u
                    method p = p
                  end
                in
                Ch_queries.Expr.( = )
                  (__q#u#query (fun u -> u#id))
                  (__q#p#query (fun p -> p#user_id))))
           (fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.nullable_scope)) ->
             object
               method u = u
               method p = p
             end))
      ~select:(fun __q ->
        object
          method user_id = __q#u#query (fun u -> u#id)
          method user_name = __q#p#query (fun p -> p#name)
        end)
  
  let sql, _parse_row =
    let open Ch_queries in
    query q @@ fun __q -> Row.int (__q#q#query (fun q -> q#user_id))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1 FROM (SELECT u.id AS _1 FROM public.users AS u) AS q

optional join with a table (in use):
  $ ./compile_and_run '
  > let q = {%q|
  >   SELECT u.id AS user_id, p.name AS user_name
  >   FROM public.users AS u
  >   LEFT JOIN OPTIONAL public.profiles AS p
  >   ON u.id = p.user_id
  > |};;
  > let sql, _parse_row = Ch_queries.(query q @@ fun __q -> Row.string_opt {%e|q.user_name|});;
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let q =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           ((Ch_queries.left_join ~optional:true)
              (Ch_queries.from (Ch_database.Public.users ~alias:"u" ~final:false))
              (Ch_database.Public.profiles ~alias:"p" ~final:false)
              ~on:(fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.scope)) ->
                let __q =
                  object
                    method u = u
                    method p = p
                  end
                in
                Ch_queries.Expr.( = )
                  (__q#u#query (fun u -> u#id))
                  (__q#p#query (fun p -> p#user_id))))
           (fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.nullable_scope)) ->
             object
               method u = u
               method p = p
             end))
      ~select:(fun __q ->
        object
          method user_id = __q#u#query (fun u -> u#id)
          method user_name = __q#p#query (fun p -> p#name)
        end)
  
  let sql, _parse_row =
    let open Ch_queries in
    query q @@ fun __q -> Row.string_opt (__q#q#query (fun q -> q#user_name))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT p.name AS _1
    FROM public.users AS u LEFT JOIN public.profiles AS p ON u.id = p.user_id)
    AS q

optional join with a subquery (elimination):
  $ ./compile_and_run '
  > let q = {%q|
  >   SELECT u.id AS user_id, p.name AS user_name
  >   FROM public.users AS u
  >   LEFT JOIN OPTIONAL (SELECT p.user_id AS user_id, p.name AS name FROM public.profiles AS p) AS p
  >   ON u.id = p.user_id
  > |};;
  > let sql, _parse_row = Ch_queries.(query q @@ fun __q -> Row.int [%e "q.user_id"]);;
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let q =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           ((Ch_queries.left_join ~optional:true)
              (Ch_queries.from (Ch_database.Public.users ~alias:"u" ~final:false))
              (Ch_queries.from_select
                 (Ch_queries.select ()
                    ~from:
                      (Ch_queries.map_from_scope
                         (Ch_queries.from
                            (Ch_database.Public.profiles ~alias:"p" ~final:false))
                         (fun (p : _ Ch_queries.scope) ->
                           object
                             method p = p
                           end))
                    ~select:(fun __q ->
                      object
                        method user_id = __q#p#query (fun p -> p#user_id)
                        method name = __q#p#query (fun p -> p#name)
                      end))
                 ~alias:"p")
              ~on:(fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.scope)) ->
                let __q =
                  object
                    method u = u
                    method p = p
                  end
                in
                Ch_queries.Expr.( = )
                  (__q#u#query (fun u -> u#id))
                  (__q#p#query (fun p -> p#user_id))))
           (fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.nullable_scope)) ->
             object
               method u = u
               method p = p
             end))
      ~select:(fun __q ->
        object
          method user_id = __q#u#query (fun u -> u#id)
          method user_name = __q#p#query (fun p -> p#name)
        end)
  
  let sql, _parse_row =
    let open Ch_queries in
    query q @@ fun __q -> Row.int (__q#q#query (fun q -> q#user_id))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1 FROM (SELECT u.id AS _1 FROM public.users AS u) AS q

optional join with a subquery (in use):
  $ ./compile_and_run '
  > let q = {%q|
  >   SELECT u.id AS user_id, p.name AS user_name
  >   FROM public.users AS u
  >   LEFT JOIN OPTIONAL (SELECT p.user_id AS user_id, p.name AS name FROM public.profiles AS p) AS p
  >   ON u.id = p.user_id
  > |};;
  > let sql, _parse_row = Ch_queries.(query q @@ fun __q -> Row.string_opt [%e "q.user_name"]);;
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let q =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           ((Ch_queries.left_join ~optional:true)
              (Ch_queries.from (Ch_database.Public.users ~alias:"u" ~final:false))
              (Ch_queries.from_select
                 (Ch_queries.select ()
                    ~from:
                      (Ch_queries.map_from_scope
                         (Ch_queries.from
                            (Ch_database.Public.profiles ~alias:"p" ~final:false))
                         (fun (p : _ Ch_queries.scope) ->
                           object
                             method p = p
                           end))
                    ~select:(fun __q ->
                      object
                        method user_id = __q#p#query (fun p -> p#user_id)
                        method name = __q#p#query (fun p -> p#name)
                      end))
                 ~alias:"p")
              ~on:(fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.scope)) ->
                let __q =
                  object
                    method u = u
                    method p = p
                  end
                in
                Ch_queries.Expr.( = )
                  (__q#u#query (fun u -> u#id))
                  (__q#p#query (fun p -> p#user_id))))
           (fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.nullable_scope)) ->
             object
               method u = u
               method p = p
             end))
      ~select:(fun __q ->
        object
          method user_id = __q#u#query (fun u -> u#id)
          method user_name = __q#p#query (fun p -> p#name)
        end)
  
  let sql, _parse_row =
    let open Ch_queries in
    query q @@ fun __q -> Row.string_opt (__q#q#query (fun q -> q#user_name))
  
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
