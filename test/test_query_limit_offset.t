LIMIT with literal value:
  $ ./compile_and_run '
  > let users = [%query "SELECT users.x FROM public.users LIMIT 1"];;
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
      ~limit:(fun (users : _ Queries.scope) -> Queries.int 1)
  
  let sql, () =
    Queries.use users @@ fun users ->
    ignore [ users#query (fun users -> users#_1) ]
  
  let () = print_endline sql
  >>> RUNNING
  SELECT users.x AS _1 FROM public.users AS users LIMIT 1

LIMIT with parameter:
  $ ./compile_and_run '
  > let users ~n = [%query "SELECT users.x FROM public.users LIMIT ?n"];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users ~n =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users"))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method _1 = users#query (fun users -> users#x)
        end)
      ~limit:(fun (users : _ Queries.scope) -> n users)
  >>> RUNNING
  val users :
    n:(< id : (Queries.non_null, int Queries.number) Queries.expr;
         is_active : (Queries.non_null, bool) Queries.expr;
         x : (Queries.non_null, string) Queries.expr >
       Queries.scope -> ('a, int Queries.number) Queries.expr) ->
    < _1 : (Queries.non_null, string) Queries.expr > Queries.scope
    Queries.select

OFFSET with literal value:
  $ ./compile_and_run '
  > let users = [%query "SELECT users.x FROM public.users OFFSET 1"];;
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
      ~offset:(fun (users : _ Queries.scope) -> Queries.int 1)
  
  let sql, () =
    Queries.use users @@ fun users ->
    ignore [ users#query (fun users -> users#_1) ]
  
  let () = print_endline sql
  >>> RUNNING
  SELECT users.x AS _1 FROM public.users AS users OFFSET 1

OFFSET with parameter:
  $ ./compile_and_run '
  > let users ~n = [%query "SELECT users.x FROM public.users OFFSET ?n"];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users ~n =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users"))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method _1 = users#query (fun users -> users#x)
        end)
      ~offset:(fun (users : _ Queries.scope) -> n users)
  >>> RUNNING
  val users :
    n:(< id : (Queries.non_null, int Queries.number) Queries.expr;
         is_active : (Queries.non_null, bool) Queries.expr;
         x : (Queries.non_null, string) Queries.expr >
       Queries.scope -> ('a, int Queries.number) Queries.expr) ->
    < _1 : (Queries.non_null, string) Queries.expr > Queries.scope
    Queries.select
