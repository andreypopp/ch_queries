LIMIT with literal value:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x FROM public.users LIMIT 1"];;
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
             object
               method users = users
             end))
      ~select:(fun __q ->
        object
          method x = __q#users#query (fun __q -> __q#x)
        end)
      ~limit:(fun __q -> Ch_queries.int 1)
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1 FROM (SELECT users.x AS _1 FROM public.users AS users LIMIT 1) AS q

LIMIT with parameter:
  $ ./compile_and_run '
  > let users ~n = [%q "SELECT users.x FROM public.users LIMIT $n"];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users ~n =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from
              (Ch_database.Public.users ~alias:"users" ~final:false))
           (fun (users : _ Ch_queries.scope) ->
             object
               method users = users
             end))
      ~select:(fun __q ->
        object
          method x = __q#users#query (fun __q -> __q#x)
        end)
      ~limit:(fun __q -> n __q)
  >>> RUNNING
  val users :
    n:(< users : < id : (Ch_queries.non_null, int Ch_queries.number)
                        Ch_queries.expr;
                   is_active : (Ch_queries.non_null, bool) Ch_queries.expr;
                   x : (Ch_queries.non_null, string) Ch_queries.expr;
                   xs : (Ch_queries.non_null,
                         (Ch_queries.non_null, string) Ch_queries.array)
                        Ch_queries.expr >
                 Ch_queries.scope > ->
       ('a, int Ch_queries.number) Ch_queries.expr) ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

OFFSET with literal value:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x FROM public.users OFFSET 1"];;
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
             object
               method users = users
             end))
      ~select:(fun __q ->
        object
          method x = __q#users#query (fun __q -> __q#x)
        end)
      ~offset:(fun __q -> Ch_queries.int 1)
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1 FROM (SELECT users.x AS _1 FROM public.users AS users OFFSET 1) AS q

OFFSET with parameter:
  $ ./compile_and_run '
  > let users ~n = [%q "SELECT users.x FROM public.users OFFSET $n"];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users ~n =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from
              (Ch_database.Public.users ~alias:"users" ~final:false))
           (fun (users : _ Ch_queries.scope) ->
             object
               method users = users
             end))
      ~select:(fun __q ->
        object
          method x = __q#users#query (fun __q -> __q#x)
        end)
      ~offset:(fun __q -> n __q)
  >>> RUNNING
  val users :
    n:(< users : < id : (Ch_queries.non_null, int Ch_queries.number)
                        Ch_queries.expr;
                   is_active : (Ch_queries.non_null, bool) Ch_queries.expr;
                   x : (Ch_queries.non_null, string) Ch_queries.expr;
                   xs : (Ch_queries.non_null,
                         (Ch_queries.non_null, string) Ch_queries.array)
                        Ch_queries.expr >
                 Ch_queries.scope > ->
       ('a, int Ch_queries.number) Ch_queries.expr) ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select
