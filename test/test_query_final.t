select from table with FINAL keyword:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x FROM public.users AS users FINAL WHERE users.is_active"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from (Ch_database.Public.users ~alias:"users" ~final:true))
           (fun (users : _ Ch_queries.scope) ->
             let __q =
               object
                 method users = users
               end
             in
             object
               method users = users
               method x = __q#users#query ?alias:(Some "x") (fun __q -> __q#x)
             end))
      ~select:(fun __q ->
        object
          method x = __q#x
        end)
      ~where:(fun __q ->
        __q#users#query ?alias:(Some "is_active") (fun __q -> __q#is_active))
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query ?alias:(Some "x") (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT users.x AS x FROM public.users AS users FINAL WHERE users.is_active

if FINAL keyword is applied to param, then it expects the table:
  $ ./compile_and_run '
  > let users table = [%q "SELECT users.x AS x FROM $table AS users FINAL WHERE users.is_active"];;
  > let users = users Ch_database.Public.users;;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users table =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from
              ((table : final:bool -> alias:string -> _ Ch_queries.from_one)
                 ~final:true ~alias:"users"))
           (fun (users : _ Ch_queries.scope) ->
             let __q =
               object
                 method users = users
               end
             in
             object
               method users = users
               method x = __q#users#query ?alias:(Some "x") (fun __q -> __q#x)
             end))
      ~select:(fun __q ->
        object
          method x = __q#x
        end)
      ~where:(fun __q ->
        __q#users#query ?alias:(Some "is_active") (fun __q -> __q#is_active))
  
  let users = users Ch_database.Public.users
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query ?alias:(Some "x") (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT users.x AS x FROM public.users AS users FINAL WHERE users.is_active
