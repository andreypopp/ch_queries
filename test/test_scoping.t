  $ ./compile_and_run '
  > let q = {%q|
  >   SELECT x.x AS x FROM public.users AS x
  > |};;
  > let sql, _parse_row = Ch_queries.query q @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"];;
  > print_endline sql;;
  > '
  >>> PREPROCESSING
  let q =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from (Ch_database.Public.users ~alias:"x" ~final:false))
           (fun (x : _ Ch_queries.scope) ->
             let __q =
               object
                 method x = x
               end
             in
             object
               method x = __q#x#query (fun __q -> __q#x)
             end))
      ~select:(fun __q ->
        object
          method x = __q#x
        end)
  
  let sql, _parse_row =
    Ch_queries.query q @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query (fun __q -> __q#x))
  ;;
  
  print_endline sql
  >>> RUNNING
  SELECT q.x FROM (SELECT x.x AS x FROM public.users AS x) AS q
