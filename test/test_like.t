test IN expression with subquery:
  $ ./compile_and_run "
  > let users = [%q \"SELECT users.x AS x FROM public.users WHERE (users.x LIKE '123%')\"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e \"q.x\"]
  > let () = print_endline sql;;
  > "
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
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
             end))
      ~select:(fun __q ->
        object
          method x = __q#x
        end)
      ~where:(fun __q ->
        Ch_queries.Expr.like
          (__q#users#query (fun __q -> __q#x))
          (Ch_queries.string "123%"))
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users.x AS _1 FROM public.users AS users WHERE like(users.x, '123%'))
    AS q



