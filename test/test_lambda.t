test IN expression with subquery:
  $ ./compile_and_run '
  > let users = [%q "SELECT length(arrayFilter(x -> x = 1, users.xs)) AS x FROM public.users"];;
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
             let __q =
               object
                 method users = users
               end
             in
             object
               method users = users
  
               method x =
                 Ch_queries.Expr.length
                   (Ch_queries.Expr.arrayFilter
                      (Ch_queries.lambda "x" (fun _ ->
                           Ch_queries.Expr.( = ) (Ch_queries.unsafe "x")
                             (Ch_queries.int 1)))
                      (__q#users#query (fun __q -> __q#xs)))
             end))
      ~select:(fun __q ->
        object
          method x = __q#x
        end)
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT length(arrayFilter((x -> (x = 1)), users.xs)) AS _1
  FROM public.users AS users

