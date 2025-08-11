GROUP BY single column:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x FROM public.users GROUP BY users.x"];;
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
      ~group_by:(fun (users : _ Ch_queries.scope) ->
        List.concat [ [ Ch_queries.A_expr (users#query (fun users -> users#x)) ] ])
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun users ->
    Ch_queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (SELECT users.x AS _1 FROM public.users AS users GROUP BY users.x) AS q

GROUP BY multiple columns:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x FROM public.users GROUP BY users.x, users.id"];;
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
      ~group_by:(fun (users : _ Ch_queries.scope) ->
        List.concat
          [
            [ Ch_queries.A_expr (users#query (fun users -> users#x)) ];
            [ Ch_queries.A_expr (users#query (fun users -> users#id)) ];
          ])
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun users ->
    Ch_queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users.x AS _1 FROM public.users AS users GROUP BY users.x, users.id)
    AS q

GROUP BY with a parameter:
  $ ./compile_and_run '
  > let users ~dimension = [%q "SELECT users.x AS x FROM public.users GROUP BY users.id, ?dimension..."];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users ~dimension =
    Ch_queries.select ()
      ~from:
        (Ch_queries.from (Ch_database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~group_by:(fun (users : _ Ch_queries.scope) ->
        List.concat
          [
            [ Ch_queries.A_expr (users#query (fun users -> users#id)) ];
            dimension users;
          ])
  >>> RUNNING
  val users :
    dimension:(< id : (Ch_queries.non_null, int Ch_queries.number)
                      Ch_queries.expr;
                 is_active : (Ch_queries.non_null, bool) Ch_queries.expr;
                 x : (Ch_queries.non_null, string) Ch_queries.expr;
                 xs : (Ch_queries.non_null,
                       (Ch_queries.non_null, string) Ch_queries.array)
                      Ch_queries.expr >
               Ch_queries.scope -> Ch_queries.a_expr list) ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select
