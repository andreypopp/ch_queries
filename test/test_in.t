test IN expression with subquery:
  $ ./compile_and_run '
  > let users = [%query "SELECT users.x AS x FROM public.users WHERE users.id IN (SELECT users.id FROM public.users)"];;
  > let sql, _parse_row = Queries.query users @@ fun users -> Queries.Row.string [%expr "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~where:(fun (users : _ Queries.scope) ->
        Queries.in_
          (users#query (fun users -> users#id))
          (Queries.In_query
             (Queries.select ()
                ~from:
                  (Queries.from
                     (Database.Public.users ~alias:"users" ~final:false))
                ~select:(fun (users : _ Queries.scope) ->
                  object
                    method _1 = users#query (fun users -> users#id)
                  end))))
  
  let sql, _parse_row =
    Queries.query users @@ fun users ->
    Queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users.x AS _1
    FROM public.users AS users
    WHERE users.id IN (SELECT users.id AS _1 FROM public.users AS users)) AS q

test IN expression with expression::
  $ ./compile_and_run '
  > let users = [%query "SELECT users.x AS x FROM public.users WHERE users.id IN [1, 2]"];;
  > let sql, _parse_row = Queries.query users @@ fun users -> Queries.Row.string [%expr "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~where:(fun (users : _ Queries.scope) ->
        Queries.in_
          (users#query (fun users -> users#id))
          (Queries.In_array (Queries.array [ Queries.int 1; Queries.int 2 ])))
  
  let sql, _parse_row =
    Queries.query users @@ fun users ->
    Queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users.x AS _1 FROM public.users AS users WHERE users.id IN [1, 2]) AS q

test IN expression with parameter:
  $ ./compile_and_run '
  > let users ~ids = [%query "SELECT users.x AS x FROM public.users WHERE users.id IN ?ids"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users ~ids =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~where:(fun (users : _ Queries.scope) ->
        Queries.in_ (users#query (fun users -> users#id)) ids)
  >>> RUNNING
  val users :
    ids:int Queries.number Queries.in_rhs ->
    < x : (Queries.non_null, string) Queries.expr > Queries.scope
    Queries.select
