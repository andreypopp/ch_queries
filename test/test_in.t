test IN expression with subquery:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x FROM public.users WHERE users.id IN (SELECT users.id AS _1 FROM public.users)"];;
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
      ~where:(fun (users : _ Ch_queries.scope) ->
        Ch_queries.in_
          (users#query (fun users -> users#id))
          (Ch_queries.In_query
             (Ch_queries.select ()
                ~from:
                  (Ch_queries.from
                     (Database.Public.users ~alias:"users" ~final:false))
                ~select:(fun (users : _ Ch_queries.scope) ->
                  object
                    method _1 = users#query (fun users -> users#id)
                  end))))
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun users ->
    Ch_queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users.x AS _1
    FROM public.users AS users
    WHERE users.id IN (SELECT users.id AS _1 FROM public.users AS users)) AS q

test IN expression with expression::
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x FROM public.users WHERE users.id IN [1, 2]"];;
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
      ~where:(fun (users : _ Ch_queries.scope) ->
        Ch_queries.in_
          (users#query (fun users -> users#id))
          (Ch_queries.In_array
             (Ch_queries.array [ Ch_queries.int 1; Ch_queries.int 2 ])))
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun users ->
    Ch_queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users.x AS _1 FROM public.users AS users WHERE users.id IN [1, 2]) AS q

test IN expression with parameter:
  $ ./compile_and_run '
  > let users ~ids = [%q "SELECT users.x AS x FROM public.users WHERE users.id IN ?ids"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users ~ids =
    Ch_queries.select ()
      ~from:(Ch_queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~where:(fun (users : _ Ch_queries.scope) ->
        Ch_queries.in_ (users#query (fun users -> users#id)) ids)
  >>> RUNNING
  val users :
    ids:int Ch_queries.number Ch_queries.in_rhs ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select
