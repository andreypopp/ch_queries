test IN expression with subquery:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x FROM public.users WHERE users.id IN (SELECT users.id AS _1 FROM public.users)"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.string [%e "q.x"]
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
      ~where:(fun __q ->
        Ch_queries.in_
          (__q#users#query (fun __q -> __q#id))
          (Ch_queries.In_query
             (Ch_queries.select ()
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
                    method _1 = __q#users#query (fun __q -> __q#id)
                  end))))
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.string (__q#q#query (fun __q -> __q#x))
  
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
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.string [%e "q.x"]
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
      ~where:(fun __q ->
        Ch_queries.in_
          (__q#users#query (fun __q -> __q#id))
          (Ch_queries.In_array
             (Ch_queries.array [ Ch_queries.int 1; Ch_queries.int 2 ])))
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.string (__q#q#query (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users.x AS _1 FROM public.users AS users WHERE users.id IN [1, 2]) AS q

test IN expression with parameter:
  $ ./compile_and_run '
  > let users ~ids = [%q "SELECT users.x AS x FROM public.users WHERE users.id IN $ids"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users ~ids =
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
      ~where:(fun __q -> Ch_queries.in_ (__q#users#query (fun __q -> __q#id)) ids)
  >>> RUNNING
  val users :
    ids:int Ch_queries.number Ch_queries.in_rhs ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select
