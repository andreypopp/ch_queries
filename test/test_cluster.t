test cluster syntax parsing:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x FROM cluster(my_cluster, view(SELECT users.x AS x, users.is_active AS is_active FROM public.users)) AS users WHERE users.is_active"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun users -> Ch_queries.Row.string [%e "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.from
           (Ch_queries.from_select ~cluster_name:"my_cluster"
              (Ch_queries.select ()
                 ~from:
                   (Ch_queries.from
                      (Database.Public.users ~alias:"users" ~final:false))
                 ~select:(fun (users : _ Ch_queries.scope) ->
                   object
                     method x = users#query (fun users -> users#x)
                     method is_active = users#query (fun users -> users#is_active)
                   end))
              ~alias:"users"))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~where:(fun (users : _ Ch_queries.scope) ->
        users#query (fun users -> users#is_active))
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun users ->
    Ch_queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users._2 AS _1
    FROM cluster(my_cluster, view(
      SELECT users.is_active AS _1, users.x AS _2 FROM public.users AS users))
      AS users
    WHERE users._1) AS q

test parameterized cluster syntax:
  $ ./compile_and_run '
  > let users cluster_name = [%q "SELECT users.x AS x FROM cluster(?cluster_name, view(SELECT users.x AS x, users.is_active AS is_active FROM public.users)) AS users WHERE users.is_active"];;
  > let sql, _parse_row = Ch_queries.query (users "test_cluster") @@ fun users -> Ch_queries.Row.string [%e "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users cluster_name =
    Ch_queries.select ()
      ~from:
        (Ch_queries.from
           (Ch_queries.from_select ~cluster_name
              (Ch_queries.select ()
                 ~from:
                   (Ch_queries.from
                      (Database.Public.users ~alias:"users" ~final:false))
                 ~select:(fun (users : _ Ch_queries.scope) ->
                   object
                     method x = users#query (fun users -> users#x)
                     method is_active = users#query (fun users -> users#is_active)
                   end))
              ~alias:"users"))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~where:(fun (users : _ Ch_queries.scope) ->
        users#query (fun users -> users#is_active))
  
  let sql, _parse_row =
    Ch_queries.query (users "test_cluster") @@ fun users ->
    Ch_queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users._2 AS _1
    FROM cluster(test_cluster, view(
      SELECT users.is_active AS _1, users.x AS _2 FROM public.users AS users))
      AS users
    WHERE users._1) AS q
