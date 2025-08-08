test IN expression with subquery:
  $ ./compile_and_run '
  > let users = [%q "SELECT length(arrayFilter(x -> x = 1, users.xs)) AS x FROM public.users"];;
  > let sql, _parse_row = Queries.query users @@ fun users -> Queries.Row.int [%e "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method x =
            Queries.Expr.length
              (Queries.Expr.arrayFilter
                 (Queries.lambda "x" (fun x ->
                      Queries.Expr.( = ) (Queries.unsafe "x") (Queries.int 1)))
                 (users#query (fun users -> users#xs)))
        end)
  
  let sql, _parse_row =
    Queries.query users @@ fun users ->
    Queries.Row.int (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT length(arrayFilter(x -> x = 1, users.xs)) AS _1
    FROM public.users AS users) AS q

