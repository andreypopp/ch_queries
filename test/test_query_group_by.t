GROUP BY single column:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x FROM public.users GROUP BY users.x"];;
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
          method x = __q#users#query (fun users -> users#x)
        end)
      ~group_by:(fun __q ->
        List.concat
          [ [ Ch_queries.A_expr (__q#users#query (fun users -> users#x)) ] ])
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.string (__q#q#query (fun q -> q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (SELECT users.x AS _1 FROM public.users AS users GROUP BY users.x) AS q

GROUP BY multiple columns:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x FROM public.users GROUP BY users.x, users.id"];;
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
          method x = __q#users#query (fun users -> users#x)
        end)
      ~group_by:(fun __q ->
        List.concat
          [
            [ Ch_queries.A_expr (__q#users#query (fun users -> users#x)) ];
            [ Ch_queries.A_expr (__q#users#query (fun users -> users#id)) ];
          ])
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.string (__q#q#query (fun q -> q#x))
  
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
        (Ch_queries.map_from_scope
           (Ch_queries.from
              (Ch_database.Public.users ~alias:"users" ~final:false))
           (fun (users : _ Ch_queries.scope) ->
             object
               method users = users
             end))
      ~select:(fun __q ->
        object
          method x = __q#users#query (fun users -> users#x)
        end)
      ~group_by:(fun __q ->
        List.concat
          [
            [ Ch_queries.A_expr (__q#users#query (fun users -> users#id)) ];
            dimension __q;
          ])
  >>> RUNNING
  val users :
    dimension:(< users : < id : (Ch_queries.non_null, int Ch_queries.number)
                                Ch_queries.expr;
                           is_active : (Ch_queries.non_null, bool)
                                       Ch_queries.expr;
                           x : (Ch_queries.non_null, string) Ch_queries.expr;
                           xs : (Ch_queries.non_null,
                                 (Ch_queries.non_null, string) Ch_queries.array)
                                Ch_queries.expr >
                         Ch_queries.scope > ->
               Ch_queries.a_expr list) ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

GROUP BY GROUPING SETS:
  $ ./compile_and_run '
  > let group_by __q = Ch_queries.grouping_sets [];;
  > let users = [%q "SELECT 1 as one FROM public.users GROUP BY ?group_by"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.int [%e "q.one"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let group_by __q = Ch_queries.grouping_sets []
  
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
          method one = Ch_queries.int 1
        end)
      ~group_by:(fun __q -> List.concat [ [ Ch_queries.A_expr (group_by __q) ] ])
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.int (__q#q#query (fun q -> q#one))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (SELECT 1 AS _1 FROM public.users AS users GROUP BY GROUPING SETS ()) AS q

  $ ./compile_and_run '
  > let group_by (__q : < u : _ Ch_queries.scope>) = Ch_queries.grouping_sets [[A_expr {%e|u.x|}; A_expr {%e|u.id|}]; [A_expr {%e|u.id|}]];;
  > let users = [%q "SELECT 1 as one FROM public.users AS u GROUP BY ?group_by"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.int [%e "q.one"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let group_by (__q : < u : _ Ch_queries.scope >) =
    Ch_queries.grouping_sets
      [
        [
          A_expr (__q#u#query (fun u -> u#x));
          A_expr (__q#u#query (fun u -> u#id));
        ];
        [ A_expr (__q#u#query (fun u -> u#id)) ];
      ]
  
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from (Ch_database.Public.users ~alias:"u" ~final:false))
           (fun (u : _ Ch_queries.scope) ->
             object
               method u = u
             end))
      ~select:(fun __q ->
        object
          method one = Ch_queries.int 1
        end)
      ~group_by:(fun __q -> List.concat [ [ Ch_queries.A_expr (group_by __q) ] ])
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.int (__q#q#query (fun q -> q#one))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT 1 AS _1
    FROM public.users AS u
    GROUP BY GROUPING SETS ((u.x, u.id), (u.id))) AS q
