GROUP BY single column:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x FROM public.users GROUP BY users.x"];;
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
             let __q =
               object
                 method users = users
               end
             in
             object
               method users = users
               method x = __q#users#query ?alias:(Some "x") (fun __q -> __q#x)
             end))
      ~select:(fun __q ->
        object
          method x = __q#x
        end)
      ~group_by:(fun __q ->
        List.concat
          [
            [
              Ch_queries.A_expr
                (__q#users#query ?alias:(Some "x") (fun __q -> __q#x));
            ];
          ])
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query ?alias:(Some "x") (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT users.x AS x FROM public.users AS users GROUP BY users.x

GROUP BY multiple columns:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x FROM public.users GROUP BY users.x, users.id"];;
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
             let __q =
               object
                 method users = users
               end
             in
             object
               method users = users
               method x = __q#users#query ?alias:(Some "x") (fun __q -> __q#x)
             end))
      ~select:(fun __q ->
        object
          method x = __q#x
        end)
      ~group_by:(fun __q ->
        List.concat
          [
            [
              Ch_queries.A_expr
                (__q#users#query ?alias:(Some "x") (fun __q -> __q#x));
            ];
            [
              Ch_queries.A_expr
                (__q#users#query ?alias:(Some "id") (fun __q -> __q#id));
            ];
          ])
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query ?alias:(Some "x") (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT users.x AS x FROM public.users AS users GROUP BY users.x, users.id

GROUP BY with a parameter:
  $ ./compile_and_run '
  > let users ~dimension = [%q "SELECT users.x AS x FROM public.users GROUP BY users.id, $.dimension..."];;
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
             let __q =
               object
                 method users = users
               end
             in
             object
               method users = users
               method x = __q#users#query ?alias:(Some "x") (fun __q -> __q#x)
             end))
      ~select:(fun __q ->
        object
          method x = __q#x
        end)
      ~group_by:(fun __q ->
        List.concat
          [
            [
              Ch_queries.A_expr
                (__q#users#query ?alias:(Some "id") (fun __q -> __q#id));
            ];
            dimension __q;
          ])
  >>> RUNNING
  val users :
    dimension:(< users : Ch_database.Public.users Ch_queries.scope;
                 x : (Ch_queries.non_null, string) Ch_queries.expr > ->
               Ch_queries.a_expr list) ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

GROUP BY GROUPING SETS:
  $ ./compile_and_run '
  > let group_by __q = Ch_queries.grouping_sets [];;
  > let users = [%q "SELECT 1 as one FROM public.users GROUP BY $.group_by"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.one"]
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
             let __q =
               object
                 method users = users
               end
             in
             object
               method users = users
               method one = Ch_queries.int 1
             end))
      ~select:(fun __q ->
        object
          method one = __q#one
        end)
      ~group_by:(fun __q -> List.concat [ [ Ch_queries.A_expr (group_by __q) ] ])
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query ?alias:(Some "one") (fun __q -> __q#one))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT 1 AS one FROM public.users AS users GROUP BY GROUPING SETS ()

  $ ./compile_and_run '
  > let group_by (__q : < u : _ Ch_queries.scope; .. >) = Ch_queries.grouping_sets [[A_expr {%e|u.x|}; A_expr {%e|u.id|}]; [A_expr {%e|u.id|}]];;
  > let users = [%q "SELECT 1 as one FROM public.users AS u GROUP BY $.group_by"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.one"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let group_by (__q : < u : _ Ch_queries.scope ; .. >) =
    Ch_queries.grouping_sets
      [
        [
          A_expr (__q#u#query ?alias:(Some "x") (fun __q -> __q#x));
          A_expr (__q#u#query ?alias:(Some "id") (fun __q -> __q#id));
        ];
        [ A_expr (__q#u#query ?alias:(Some "id") (fun __q -> __q#id)) ];
      ]
  
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from (Ch_database.Public.users ~alias:"u" ~final:false))
           (fun (u : _ Ch_queries.scope) ->
             let __q =
               object
                 method u = u
               end
             in
             object
               method u = u
               method one = Ch_queries.int 1
             end))
      ~select:(fun __q ->
        object
          method one = __q#one
        end)
      ~group_by:(fun __q -> List.concat [ [ Ch_queries.A_expr (group_by __q) ] ])
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query ?alias:(Some "one") (fun __q -> __q#one))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT 1 AS one
  FROM public.users AS u
  GROUP BY GROUPING SETS ((u.x, u.id), (u.id))

GROUP BY can refer to SELECTed columns:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x FROM public.users GROUP BY x"];;
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
             let __q =
               object
                 method users = users
               end
             in
             object
               method users = users
               method x = __q#users#query ?alias:(Some "x") (fun __q -> __q#x)
             end))
      ~select:(fun __q ->
        object
          method x = __q#x
        end)
      ~group_by:(fun __q -> List.concat [ [ Ch_queries.A_expr __q#x ] ])
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query ?alias:(Some "x") (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT users.x AS x FROM public.users AS users GROUP BY users.x
