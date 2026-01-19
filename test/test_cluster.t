test cluster syntax parsing:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x FROM cluster(my_cluster, view(SELECT users.x AS x, users.is_active AS is_active FROM public.users)) AS users WHERE users.is_active"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from
              (Ch_queries.from_select ~cluster_name:"my_cluster"
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
  
                             method is_active =
                               __q#users#query (fun __q -> __q#is_active)
                           end))
                    ~select:(fun __q ->
                      object
                        method x = __q#x
                        method is_active = __q#is_active
                      end))
                 ~alias:"users"))
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
        end)
      ~where:(fun __q -> __q#users#query (fun __q -> __q#is_active))
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q.x
  FROM (
    SELECT users.x AS x
    FROM cluster(my_cluster, view(
      SELECT users.is_active AS is_active, users.x AS x FROM public.users AS users))
      AS users
    WHERE users.is_active) AS q

test parameterized cluster syntax:
  $ ./compile_and_run '
  > let users cluster_name = [%q "SELECT users.x AS x FROM cluster($cluster_name, view(SELECT users.x AS x, users.is_active AS is_active FROM public.users)) AS users WHERE users.is_active"];;
  > let sql, _parse_row = Ch_queries.query (users "test_cluster") @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users cluster_name =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from
              (Ch_queries.from_select ~cluster_name
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
  
                             method is_active =
                               __q#users#query (fun __q -> __q#is_active)
                           end))
                    ~select:(fun __q ->
                      object
                        method x = __q#x
                        method is_active = __q#is_active
                      end))
                 ~alias:"users"))
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
        end)
      ~where:(fun __q -> __q#users#query (fun __q -> __q#is_active))
  
  let sql, _parse_row =
    Ch_queries.query (users "test_cluster") @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q.x
  FROM (
    SELECT users.x AS x
    FROM cluster(test_cluster, view(
      SELECT users.is_active AS is_active, users.x AS x FROM public.users AS users))
      AS users
    WHERE users.is_active) AS q
