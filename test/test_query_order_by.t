ORDER BY single column (default ASC):
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x FROM public.users ORDER BY users.x"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun users -> Ch_queries.Row.string [%e "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.from (Ch_database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~order_by:(fun (users : _ Ch_queries.scope) ->
        List.concat
          [ [ (Ch_queries.A_expr (users#query (fun users -> users#x)), `ASC) ] ])
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun users ->
    Ch_queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (SELECT users.x AS _1 FROM public.users AS users ORDER BY users.x ASC) AS q

ORDER BY with DESC:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x FROM public.users ORDER BY users.x DESC"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun users -> Ch_queries.Row.string [%e "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.from (Ch_database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~order_by:(fun (users : _ Ch_queries.scope) ->
        List.concat
          [ [ (Ch_queries.A_expr (users#query (fun users -> users#x)), `DESC) ] ])
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun users ->
    Ch_queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users.x AS _1 FROM public.users AS users ORDER BY users.x DESC) AS q

ORDER BY multiple columns:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x FROM public.users ORDER BY users.x, users.id DESC"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun users -> Ch_queries.Row.string [%e "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.from (Ch_database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~order_by:(fun (users : _ Ch_queries.scope) ->
        List.concat
          [
            [ (Ch_queries.A_expr (users#query (fun users -> users#x)), `ASC) ];
            [ (Ch_queries.A_expr (users#query (fun users -> users#id)), `DESC) ];
          ])
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun users ->
    Ch_queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users.x AS _1
    FROM public.users AS users
    ORDER BY users.x ASC, users.id DESC) AS q

ORDER BY with a parameter:
  $ ./compile_and_run '
  > let users ~ord = [%q "SELECT users.x FROM public.users ORDER BY ?ord..."];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users ~ord =
    Ch_queries.select ()
      ~from:
        (Ch_queries.from (Ch_database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~order_by:(fun (users : _ Ch_queries.scope) -> List.concat [ ord users ])
  >>> RUNNING
  val users :
    ord:(< id : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr;
           is_active : (Ch_queries.non_null, bool) Ch_queries.expr;
           x : (Ch_queries.non_null, string) Ch_queries.expr;
           xs : (Ch_queries.non_null,
                 (Ch_queries.non_null, string) Ch_queries.array)
                Ch_queries.expr >
         Ch_queries.scope -> (Ch_queries.a_expr * [ `ASC | `DESC ]) list) ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select
