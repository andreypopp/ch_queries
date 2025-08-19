  $ ./compile_and_run '
  > let q = 
  >   Ch_queries.select ()
  >   ~from:[%f "FROM public.users JOIN public.profiles ON users.id = profiles.user_id"]
  >   ~select:(fun __q -> object method users = __q#users method profiles = __q#profiles end)
  > ;;
  > let sql, _parse_row = Ch_queries.query q Ch_queries.Row.(fun __q -> string {%e|q.(coalesce(profiles.name, users.x))|}) in
  > print_endline sql;;
  > '
  >>> PREPROCESSING
  let q =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.join
              (Ch_queries.from
                 (Ch_database.Public.users ~alias:"users" ~final:false))
              (Ch_database.Public.profiles ~alias:"profiles" ~final:false)
              ~on:(fun
                  ((users : _ Ch_queries.scope), (profiles : _ Ch_queries.scope))
                ->
                let __q =
                  object
                    method users = users
                    method profiles = profiles
                  end
                in
                Ch_queries.Expr.( = )
                  (__q#users#query (fun __q -> __q#id))
                  (__q#profiles#query (fun __q -> __q#user_id))))
           (fun ((users : _ Ch_queries.scope), (profiles : _ Ch_queries.scope)) ->
             object
               method users = users
               method profiles = profiles
             end))
      ~select:(fun __q ->
        object
          method users = __q#users
          method profiles = __q#profiles
        end)
  ;;
  
  let sql, _parse_row =
    Ch_queries.query q
      (let open Ch_queries.Row in
       fun __q ->
         string
           (__q#q#query (fun __q ->
                Ch_queries.Expr.coalesce
                  (__q#profiles#query (fun __q -> __q#name))
                  (__q#users#query (fun __q -> __q#x)))))
  in
  print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT coalesce(profiles.name, users.x) AS _1
    FROM public.users AS users
    INNER JOIN public.profiles AS profiles
    ON users.id = profiles.user_id) AS q

The param within the scope receives the scope as argument:
  $ ./compile_and_run '
  > let q1 = 
  >   Ch_queries.select ()
  >   ~from:[%f "FROM public.users JOIN public.profiles ON users.id = profiles.user_id"]
  >   ~select:(fun __q -> object method users = __q#users method profiles = __q#profiles end)
  >   |> Ch_queries.from_select
  > ;;
  > let q2 ~field =
  >   {%q|SELECT q.(?field) AS field FROM ?q1 AS q|}
  > ;;
  > #show q2;;
  > let q = q2 ~field:(fun __q -> {%e|coalesce(profiles.name, users.x)|}) in
  > let sql, _parse_row = Ch_queries.query q Ch_queries.Row.(fun __q -> string {%e|q.field|}) in
  > print_endline sql;;
  > '
  >>> PREPROCESSING
  let q1 =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.join
              (Ch_queries.from
                 (Ch_database.Public.users ~alias:"users" ~final:false))
              (Ch_database.Public.profiles ~alias:"profiles" ~final:false)
              ~on:(fun
                  ((users : _ Ch_queries.scope), (profiles : _ Ch_queries.scope))
                ->
                let __q =
                  object
                    method users = users
                    method profiles = profiles
                  end
                in
                Ch_queries.Expr.( = )
                  (__q#users#query (fun __q -> __q#id))
                  (__q#profiles#query (fun __q -> __q#user_id))))
           (fun ((users : _ Ch_queries.scope), (profiles : _ Ch_queries.scope)) ->
             object
               method users = users
               method profiles = profiles
             end))
      ~select:(fun __q ->
        object
          method users = __q#users
          method profiles = __q#profiles
        end)
    |> Ch_queries.from_select
  
  let q2 ~field =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from (q1 ~alias:"q"))
           (fun (q : _ Ch_queries.scope) ->
             object
               method q = q
             end))
      ~select:(fun __q ->
        object
          method field = __q#q#query (fun __q -> field __q)
        end)
  ;;
  
  let q =
    q2 ~field:(fun __q ->
        Ch_queries.Expr.coalesce
          (__q#profiles#query (fun __q -> __q#name))
          (__q#users#query (fun __q -> __q#x)))
  in
  let sql, _parse_row =
    Ch_queries.query q
      (let open Ch_queries.Row in
       fun __q -> string (__q#q#query (fun __q -> __q#field)))
  in
  print_endline sql
  >>> RUNNING
  val q2 :
    field:(< profiles : < name : (Ch_queries.non_null, string) Ch_queries.expr;
                          user_id : (Ch_queries.non_null,
                                     int Ch_queries.number)
                                    Ch_queries.expr >
                        Ch_queries.scope;
             users : < id : (Ch_queries.non_null, int Ch_queries.number)
                            Ch_queries.expr;
                       is_active : (Ch_queries.non_null, bool) Ch_queries.expr;
                       x : (Ch_queries.non_null, string) Ch_queries.expr;
                       xs : (Ch_queries.non_null,
                             (Ch_queries.non_null, string) Ch_queries.array)
                            Ch_queries.expr >
                     Ch_queries.scope > ->
           ('a, 'b) Ch_queries.expr) ->
    < field : ('a, 'b) Ch_queries.expr > Ch_queries.scope Ch_queries.select
  SELECT q._1
  FROM (
    SELECT q._1 AS _1
    FROM (
      SELECT coalesce(profiles.name, users.x) AS _1
      FROM public.users AS users
      INNER JOIN public.profiles AS profiles
      ON users.id = profiles.user_id) AS q) AS q
