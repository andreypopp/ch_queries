LIMIT with literal value:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x FROM public.users LIMIT 1"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun users -> Ch_queries.Row.string [%e "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:(Ch_queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~limit:(fun (users : _ Ch_queries.scope) -> Ch_queries.int 1)
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun users ->
    Ch_queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1 FROM (SELECT users.x AS _1 FROM public.users AS users LIMIT 1) AS q

LIMIT with parameter:
  $ ./compile_and_run '
  > let users ~n = [%q "SELECT users.x FROM public.users LIMIT ?n"];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users ~n =
    Ch_queries.select ()
      ~from:(Ch_queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~limit:(fun (users : _ Ch_queries.scope) -> n users)
  >>> RUNNING
  val users :
    n:(< id : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr;
         is_active : (Ch_queries.non_null, bool) Ch_queries.expr;
         x : (Ch_queries.non_null, string) Ch_queries.expr;
         xs : (Ch_queries.non_null,
               (Ch_queries.non_null, string) Ch_queries.array)
              Ch_queries.expr >
       Ch_queries.scope -> ('a, int Ch_queries.number) Ch_queries.expr) ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

OFFSET with literal value:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x FROM public.users OFFSET 1"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun users -> Ch_queries.Row.string [%e "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:(Ch_queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~offset:(fun (users : _ Ch_queries.scope) -> Ch_queries.int 1)
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun users ->
    Ch_queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1 FROM (SELECT users.x AS _1 FROM public.users AS users OFFSET 1) AS q

OFFSET with parameter:
  $ ./compile_and_run '
  > let users ~n = [%q "SELECT users.x FROM public.users OFFSET ?n"];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users ~n =
    Ch_queries.select ()
      ~from:(Ch_queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~offset:(fun (users : _ Ch_queries.scope) -> n users)
  >>> RUNNING
  val users :
    n:(< id : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr;
         is_active : (Ch_queries.non_null, bool) Ch_queries.expr;
         x : (Ch_queries.non_null, string) Ch_queries.expr;
         xs : (Ch_queries.non_null,
               (Ch_queries.non_null, string) Ch_queries.array)
              Ch_queries.expr >
       Ch_queries.scope -> ('a, int Ch_queries.number) Ch_queries.expr) ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select
