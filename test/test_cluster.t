test cluster syntax parsing:
  $ ./compile_and_run '
  > let users = [%query "SELECT users.x AS x FROM cluster(my_cluster, view(SELECT users.x AS x, users.is_active AS is_active FROM public.users)) AS users WHERE users.is_active"];;
  > let sql, _parse_row = Queries.query users @@ fun users -> Queries.Row.string [%expr "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Queries.select ()
      ~from:
        (Queries.from
           (Queries.from_select ~cluster_name:"my_cluster"
              (Queries.select ()
                 ~from:(Queries.from (Database.Public.users ~alias:"users"))
                 ~select:(fun (users : _ Queries.scope) ->
                   object
                     method x = users#query (fun users -> users#x)
                     method is_active = users#query (fun users -> users#is_active)
                   end))
              ~alias:"users"))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~where:(fun (users : _ Queries.scope) ->
        users#query (fun users -> users#is_active))
  
  let sql, _parse_row =
    Queries.query users @@ fun users ->
    Queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users._2 AS _1
    FROM cluster(my_cluster, view(SELECT users.is_active AS _1, users.x AS _2
    FROM public.users AS users)) AS users
    WHERE users._1) AS q

test parameterized cluster syntax:
  $ ./compile_and_run '
  > let users cluster_name = [%query "SELECT users.x AS x FROM cluster(?cluster_name, view(SELECT users.x AS x, users.is_active AS is_active FROM public.users)) AS users WHERE users.is_active"];;
  > let sql, _parse_row = Queries.query (users "test_cluster") @@ fun users -> Queries.Row.string [%expr "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users cluster_name =
    Queries.select ()
      ~from:
        (Queries.from
           (Queries.from_select ~cluster_name
              (Queries.select ()
                 ~from:(Queries.from (Database.Public.users ~alias:"users"))
                 ~select:(fun (users : _ Queries.scope) ->
                   object
                     method x = users#query (fun users -> users#x)
                     method is_active = users#query (fun users -> users#is_active)
                   end))
              ~alias:"users"))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~where:(fun (users : _ Queries.scope) ->
        users#query (fun users -> users#is_active))
  
  let sql, _parse_row =
    Queries.query (users "test_cluster") @@ fun users ->
    Queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users._2 AS _1
    FROM cluster(test_cluster, view(SELECT users.is_active AS _1, users.x AS _2
    FROM public.users AS users)) AS users
    WHERE users._1) AS q
