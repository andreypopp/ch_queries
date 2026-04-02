LIMIT BY with literal values:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x FROM public.users ORDER BY users.x ASC LIMIT 2 BY users.x"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from
              (Ch_database.Public.users ~alias:"users" ~final:false))
           (fun (users : _ Ch_queries.scope) ->
             object
               method users = users
             end))
      ~select:(fun __q ->
        object
          method x = __q#users#query ?alias:(Some "x") (fun __q -> __q#x)
        end)
      ~order_by:(fun __q ->
        List.concat
          [
            [
              ( Ch_queries.A_expr
                  (__q#users#query ?alias:(Some "x") (fun __q -> __q#x)),
                `ASC,
                None );
            ];
          ])
      ~limit_by:(fun __q ->
        ( Ch_queries.int 2,
          [
            Ch_queries.A_expr
              (__q#users#query ?alias:(Some "x") (fun __q -> __q#x));
          ] ))
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query ?alias:(Some "x") (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT users.x AS x
  FROM public.users AS users
  ORDER BY users.x ASC
  LIMIT 2 BY users.x

LIMIT BY with multiple expressions:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x, users.id FROM public.users LIMIT 5 BY users.x, users.id"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from
              (Ch_database.Public.users ~alias:"users" ~final:false))
           (fun (users : _ Ch_queries.scope) ->
             object
               method users = users
             end))
      ~select:(fun __q ->
        object
          method x = __q#users#query ?alias:(Some "x") (fun __q -> __q#x)
          method id = __q#users#query ?alias:(Some "id") (fun __q -> __q#id)
        end)
      ~limit_by:(fun __q ->
        ( Ch_queries.int 5,
          [
            Ch_queries.A_expr
              (__q#users#query ?alias:(Some "x") (fun __q -> __q#x));
            Ch_queries.A_expr
              (__q#users#query ?alias:(Some "id") (fun __q -> __q#id));
          ] ))
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query ?alias:(Some "x") (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT users.x AS x FROM public.users AS users LIMIT 5 BY users.x, users.id

LIMIT BY followed by regular LIMIT:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x FROM public.users LIMIT 5 BY users.x LIMIT 100"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from
              (Ch_database.Public.users ~alias:"users" ~final:false))
           (fun (users : _ Ch_queries.scope) ->
             object
               method users = users
             end))
      ~select:(fun __q ->
        object
          method x = __q#users#query ?alias:(Some "x") (fun __q -> __q#x)
        end)
      ~limit_by:(fun __q ->
        ( Ch_queries.int 5,
          [
            Ch_queries.A_expr
              (__q#users#query ?alias:(Some "x") (fun __q -> __q#x));
          ] ))
      ~limit:(fun __q -> Ch_queries.int 100)
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query ?alias:(Some "x") (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT users.x AS x FROM public.users AS users LIMIT 5 BY users.x LIMIT 100
