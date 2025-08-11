test IN expression with subquery:
  $ ./compile_and_run '
  > let users = [%q "SELECT length(arrayFilter(x -> x = 1, users.xs)) AS x FROM public.users"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun users -> Ch_queries.Row.int [%e "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.from (Ch_database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x =
            Ch_queries.Expr.length
              (Ch_queries.Expr.arrayFilter
                 (Ch_queries.lambda "x" (fun x ->
                      Ch_queries.Expr.( = ) (Ch_queries.unsafe "x")
                        (Ch_queries.int 1)))
                 (users#query (fun users -> users#xs)))
        end)
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun users ->
    Ch_queries.Row.int (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT length(arrayFilter(x -> (x = 1), users.xs)) AS _1
    FROM public.users AS users) AS q

