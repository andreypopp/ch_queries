ORDER BY single column (default ASC):
  $ ./compile_and_run '
  > let users = [%query "SELECT users.x FROM public.users ORDER BY users.x"];;
  > let sql, () = Queries.use users @@ fun users -> ignore [[%expr "users._1"]]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users"))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method _1 = users#query (fun users -> users#x)
        end)
      ~order_by:(fun (users : _ Queries.scope) ->
        List.concat
          [ [ (Queries.A_expr (users#query (fun users -> users#x)), `ASC) ] ])
  
  let sql, () =
    Queries.use users @@ fun users ->
    ignore [ users#query (fun users -> users#_1) ]
  
  let () = print_endline sql
  >>> RUNNING
  SELECT users.x AS _1 FROM public.users AS users ORDER BY users.x ASC

ORDER BY with DESC:
  $ ./compile_and_run '
  > let users = [%query "SELECT users.x FROM public.users ORDER BY users.x DESC"];;
  > let sql, () = Queries.use users @@ fun users -> ignore [[%expr "users._1"]]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users"))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method _1 = users#query (fun users -> users#x)
        end)
      ~order_by:(fun (users : _ Queries.scope) ->
        List.concat
          [ [ (Queries.A_expr (users#query (fun users -> users#x)), `DESC) ] ])
  
  let sql, () =
    Queries.use users @@ fun users ->
    ignore [ users#query (fun users -> users#_1) ]
  
  let () = print_endline sql
  >>> RUNNING
  SELECT users.x AS _1 FROM public.users AS users ORDER BY users.x DESC

ORDER BY multiple columns:
  $ ./compile_and_run '
  > let users = [%query "SELECT users.x FROM public.users ORDER BY users.x, users.id DESC"];;
  > let sql, () = Queries.use users @@ fun users -> ignore [[%expr "users._1"]]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users"))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method _1 = users#query (fun users -> users#x)
        end)
      ~order_by:(fun (users : _ Queries.scope) ->
        List.concat
          [
            [ (Queries.A_expr (users#query (fun users -> users#x)), `ASC) ];
            [ (Queries.A_expr (users#query (fun users -> users#id)), `DESC) ];
          ])
  
  let sql, () =
    Queries.use users @@ fun users ->
    ignore [ users#query (fun users -> users#_1) ]
  
  let () = print_endline sql
  >>> RUNNING
  SELECT users.x AS _1
  FROM public.users AS users
  ORDER BY users.x ASC, users.id DESC

ORDER BY with a parameter:
  $ ./compile_and_run '
  > let users ~ord = [%query "SELECT users.x FROM public.users ORDER BY ord..."];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users ~ord =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users"))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method _1 = users#query (fun users -> users#x)
        end)
      ~order_by:(fun (users : _ Queries.scope) -> List.concat [ ord users ])
  >>> RUNNING
  val users :
    ord:(< id : (Queries.non_null, int Queries.number) Queries.expr;
           is_active : (Queries.non_null, bool) Queries.expr;
           x : (Queries.non_null, string) Queries.expr >
         Queries.scope -> (Queries.a_expr * [ `ASC | `DESC ]) list) ->
    < _1 : (Queries.non_null, string) Queries.expr > Queries.scope
    Queries.select
