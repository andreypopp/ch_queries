GROUP BY single column:
  $ ./compile_and_run '
  > let users = [%query "SELECT users.x AS x FROM public.users GROUP BY users.x"];;
  > let sql, () = Queries.use users @@ fun users -> ignore [[%expr "users.x"]]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users"))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~group_by:(fun (users : _ Queries.scope) ->
        List.concat [ [ Queries.A_expr (users#query (fun users -> users#x)) ] ])
  
  let sql, () =
    Queries.use users @@ fun users ->
    ignore [ users#query (fun users -> users#x) ]
  
  let () = print_endline sql
  >>> RUNNING
  SELECT users.x AS _1 FROM public.users AS users GROUP BY users.x

GROUP BY multiple columns:
  $ ./compile_and_run '
  > let users = [%query "SELECT users.x AS x FROM public.users GROUP BY users.x, users.id"];;
  > let sql, () = Queries.use users @@ fun users -> ignore [%expr "users.x"];;
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users"))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~group_by:(fun (users : _ Queries.scope) ->
        List.concat
          [
            [ Queries.A_expr (users#query (fun users -> users#x)) ];
            [ Queries.A_expr (users#query (fun users -> users#id)) ];
          ])
  
  let sql, () =
    Queries.use users @@ fun users -> ignore (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT users.x AS _1 FROM public.users AS users GROUP BY users.x, users.id

GROUP BY with a parameter:
  $ ./compile_and_run '
  > let users ~dimension = [%query "SELECT users.x AS x FROM public.users GROUP BY users.id, dimension..."];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users ~dimension =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users"))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~group_by:(fun (users : _ Queries.scope) ->
        List.concat
          [
            [ Queries.A_expr (users#query (fun users -> users#id)) ];
            dimension users;
          ])
  >>> RUNNING
  val users :
    dimension:(< id : (Queries.non_null, int Queries.number) Queries.expr;
                 is_active : (Queries.non_null, bool) Queries.expr;
                 x : (Queries.non_null, string) Queries.expr >
               Queries.scope -> Queries.a_expr list) ->
    < x : (Queries.non_null, string) Queries.expr > Queries.scope
    Queries.select
