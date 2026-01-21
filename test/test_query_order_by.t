ORDER BY single column (default ASC):
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x FROM public.users ORDER BY users.x"];;
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
          method x = __q#users#query (fun __q -> __q#x)
        end)
      ~order_by:(fun __q ->
        List.concat
          [
            [
              (Ch_queries.A_expr (__q#users#query (fun __q -> __q#x)), `ASC, None);
            ];
          ])
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT users.x AS x FROM public.users AS users ORDER BY users.x ASC

ORDER BY with DESC:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x FROM public.users ORDER BY users.x DESC"];;
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
          method x = __q#users#query (fun __q -> __q#x)
        end)
      ~order_by:(fun __q ->
        List.concat
          [
            [
              (Ch_queries.A_expr (__q#users#query (fun __q -> __q#x)), `DESC, None);
            ];
          ])
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT users.x AS x FROM public.users AS users ORDER BY users.x DESC

ORDER BY multiple columns:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x FROM public.users ORDER BY users.x, users.id DESC"];;
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
          method x = __q#users#query (fun __q -> __q#x)
        end)
      ~order_by:(fun __q ->
        List.concat
          [
            [
              (Ch_queries.A_expr (__q#users#query (fun __q -> __q#x)), `ASC, None);
            ];
            [
              ( Ch_queries.A_expr (__q#users#query (fun __q -> __q#id)),
                `DESC,
                None );
            ];
          ])
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT users.x AS x
  FROM public.users AS users
  ORDER BY users.x ASC, users.id DESC

ORDER BY with a parameter:
  $ ./compile_and_run '
  > let users ~ord = [%q "SELECT users.x FROM public.users ORDER BY $.ord..."];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users ~ord =
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
          method x = __q#users#query (fun __q -> __q#x)
        end)
      ~order_by:(fun __q -> List.concat [ ord __q ])
  >>> RUNNING
  val users :
    ord:(< users : Ch_database.Public.users Ch_queries.scope > ->
         Ch_queries.an_order_by list) ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select
