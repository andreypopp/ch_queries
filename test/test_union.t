Programmatic union
  $ ./compile_and_run '
  > let users1 = [%q "SELECT 1 AS x FROM public.users"];;
  > let users2 = [%q "SELECT 2 AS x FROM public.users"];;
  > let users = [%q $users1 UNION $users2"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  File "-", line 4, characters 16-17:
  4 | let users = [%q $users1 UNION $users2"];;
                      ^
  Error: Syntax error
  [1]

UNION syntax:
  $ ./compile_and_run '
  > let users = [%q "SELECT 1 AS x FROM public.users UNION SELECT 2 AS x FROM public.users"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
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
                let __q =
                  object
                    method users = users
                  end
                in
                object
                  method users = users
                  method x = Ch_queries.int 1
                end))
         ~select:(fun __q ->
           object
             method x = __q#x
           end))
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
                  method x = Ch_queries.int 2
                end))
         ~select:(fun __q ->
           object
             method x = __q#x
           end))
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT 1 AS _1 FROM public.users AS users
    UNION
    SELECT 2 AS _1 FROM public.users AS users) AS q

test edge case with dynamic scopes:
  $ ./compile_and_run '
  > let select_x __q = object
  >   method get = function | `X -> Ch_queries.string "dup" | `Y -> Ch_queries.string "X_Y" | `Z -> Ch_queries.string "dup"
  > end;;
  > let select_y __q = object
  >   method get = function | `X -> Ch_queries.string "dup" | `Y -> Ch_queries.string "dup" | `Z -> Ch_queries.string "Y_Z"
  > end;;
  > let select_z __q = object
  >   method get = function | `X -> Ch_queries.string "Z_X" | `Y -> Ch_queries.string "dup" | `Z -> Ch_queries.string "dup"
  > end;;
  > let users = [%q "SELECT $.select_x... FROM public.users UNION SELECT $.select_y... FROM public.users UNION SELECT $.select_z... FROM public.users"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> 
  >   let open Ch_queries.Row in
  >   let+ _ = Ch_queries.Row.ignore [%e "q.get(${`X})"]
  >   and+ _ = Ch_queries.Row.ignore [%e "q.get(${`Y})"]
  >   and+ _ = Ch_queries.Row.ignore [%e "q.get(${`Z})"]
  >   and+ _ = Ch_queries.Row.ignore [%e "q.get(${`Z})"]
  >   in ();;
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let select_x __q =
    object
      method get =
        function
        | `X -> Ch_queries.string "dup"
        | `Y -> Ch_queries.string "X_Y"
        | `Z -> Ch_queries.string "dup"
    end
  
  let select_y __q =
    object
      method get =
        function
        | `X -> Ch_queries.string "dup"
        | `Y -> Ch_queries.string "dup"
        | `Z -> Ch_queries.string "Y_Z"
    end
  
  let select_z __q =
    object
      method get =
        function
        | `X -> Ch_queries.string "Z_X"
        | `Y -> Ch_queries.string "dup"
        | `Z -> Ch_queries.string "dup"
    end
  
  let users =
    Ch_queries.union
      (Ch_queries.union
         (Ch_queries.select ()
            ~from:
              (Ch_queries.map_from_scope
                 (Ch_queries.from
                    (Ch_database.Public.users ~alias:"users" ~final:false))
                 (fun (users : _ Ch_queries.scope) ->
                   object
                     method users = users
                   end))
            ~select:(fun __q -> select_x __q))
         (Ch_queries.select ()
            ~from:
              (Ch_queries.map_from_scope
                 (Ch_queries.from
                    (Ch_database.Public.users ~alias:"users" ~final:false))
                 (fun (users : _ Ch_queries.scope) ->
                   object
                     method users = users
                   end))
            ~select:(fun __q -> select_y __q)))
      (Ch_queries.select ()
         ~from:
           (Ch_queries.map_from_scope
              (Ch_queries.from
                 (Ch_database.Public.users ~alias:"users" ~final:false))
              (fun (users : _ Ch_queries.scope) ->
                object
                  method users = users
                end))
         ~select:(fun __q -> select_z __q))
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    let open Ch_queries.Row in
    let+ _ = Ch_queries.Row.ignore (__q#q#query (fun __q -> __q#get `X))
    and+ _ = Ch_queries.Row.ignore (__q#q#query (fun __q -> __q#get `Y))
    and+ _ = Ch_queries.Row.ignore (__q#q#query (fun __q -> __q#get `Z))
    and+ _ = Ch_queries.Row.ignore (__q#q#query (fun __q -> __q#get `Z)) in
    ()
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1, q._2, q._3, q._3
  FROM (
    SELECT 'dup' AS _1, 'X_Y' AS _2, 'dup' AS _3 FROM public.users AS users
    UNION
    SELECT 'dup' AS _1, 'dup' AS _2, 'Y_Z' AS _3 FROM public.users AS users
    UNION
    SELECT 'Z_X' AS _1, 'dup' AS _2, 'dup' AS _3 FROM public.users AS users) AS q
