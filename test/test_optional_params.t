optional param in WHERE clause:
  $ ./compile_and_run '
  > let users ?where () = [%q "SELECT users.x AS x FROM public.users WHERE ?$.where"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users ?where () =
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
      ?where
  >>> RUNNING
  val users :
    ?where:(< users : Ch_database.Public.users Ch_queries.scope;
              x : (Ch_queries.non_null, string) Ch_queries.expr > ->
            ('a, bool) Ch_queries.expr) ->
    unit ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

optional param in HAVING clause:
  $ ./compile_and_run '
  > let users ?having () = [%q "SELECT users.x AS x FROM public.users GROUP BY users.x HAVING ?$.having"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users ?having () =
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
      ?having
  >>> RUNNING
  val users :
    ?having:(< users : Ch_database.Public.users Ch_queries.scope;
               x : (Ch_queries.non_null, string) Ch_queries.expr > ->
             ('a, bool) Ch_queries.expr) ->
    unit ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

optional param in QUALIFY clause:
  $ ./compile_and_run '
  > let users ?qualify () = [%q "SELECT users.x AS x FROM public.users QUALIFY ?$.qualify"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users ?qualify () =
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
      ?qualify
  >>> RUNNING
  val users :
    ?qualify:(< users : Ch_database.Public.users Ch_queries.scope;
                x : (Ch_queries.non_null, string) Ch_queries.expr > ->
              ('a, bool) Ch_queries.expr) ->
    unit ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

optional param in LIMIT clause:
  $ ./compile_and_run '
  > let users ?limit () = [%q "SELECT users.x AS x FROM public.users LIMIT ?$.limit"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users ?limit () =
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
      ?limit
  >>> RUNNING
  val users :
    ?limit:(< users : Ch_database.Public.users Ch_queries.scope;
              x : (Ch_queries.non_null, string) Ch_queries.expr > ->
            ('a, int Ch_queries.number) Ch_queries.expr) ->
    unit ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

optional param in OFFSET clause:
  $ ./compile_and_run '
  > let users ?offset () = [%q "SELECT users.x AS x FROM public.users OFFSET ?$.offset"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users ?offset () =
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
      ?offset
  >>> RUNNING
  val users :
    ?offset:(< users : Ch_database.Public.users Ch_queries.scope;
               x : (Ch_queries.non_null, string) Ch_queries.expr > ->
             ('a, int Ch_queries.number) Ch_queries.expr) ->
    unit ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

optional param in ORDER BY clause:
  $ ./compile_and_run '
  > let users ?order_by () = [%q "SELECT users.x AS x FROM public.users ORDER BY ?$.order_by..."];;
  > #show users
  > '
  >>> PREPROCESSING
  let users ?order_by () =
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
      ?order_by
  >>> RUNNING
  val users :
    ?order_by:(< users : Ch_database.Public.users Ch_queries.scope;
                 x : (Ch_queries.non_null, string) Ch_queries.expr > ->
               Ch_queries.an_order_by list) ->
    unit ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

optional ORDER BY with empty list omits ORDER BY clause:
  $ ./compile_and_run '
  > let users ?order_by () = [%q "SELECT users.x AS x FROM public.users ORDER BY ?$.order_by..."];;
  > (* without order_by - no ORDER BY *)
  > let sql, _ = Ch_queries.query (users ()) @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
  > let () = print_endline sql;;
  > (* with empty list - no ORDER BY *)
  > let sql, _ = Ch_queries.query (users ~order_by:(fun _ -> []) ()) @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
  > let () = print_endline sql;;
  > (* with order_by - ORDER BY is printed *)
  > let sql, _ = Ch_queries.query (users ~order_by:(fun __q -> [(Ch_queries.A_expr __q#x, `ASC, None)]) ()) @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users ?order_by () =
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
      ?order_by
  
  let sql, _ =
    Ch_queries.query (users ()) @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query ?alias:(Some "x") (fun __q -> __q#x))
  
  let () = print_endline sql
  
  let sql, _ =
    Ch_queries.query (users ~order_by:(fun _ -> []) ()) @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query ?alias:(Some "x") (fun __q -> __q#x))
  
  let () = print_endline sql
  
  let sql, _ =
    Ch_queries.query
      (users ~order_by:(fun __q -> [ (Ch_queries.A_expr __q#x, `ASC, None) ]) ())
    @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query ?alias:(Some "x") (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT users.x AS x FROM public.users AS users
  SELECT users.x AS x FROM public.users AS users
  SELECT users.x AS x FROM public.users AS users ORDER BY users.x ASC

error: optional param without scope access not allowed:
  $ ./compile_and_run '
  > let users ?where () = [%q "SELECT users.x AS x FROM public.users WHERE ?$where"];;
  > '
  >>> PREPROCESSING
  File "-", line 2, characters 71-78:
  Error: ?$where: optional parameters must use scope access syntax (?$.where)
  [1]

error: optional param not allowed in expression context:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x FROM public.users WHERE users.id = ?$.id"];;
  > '
  >>> PREPROCESSING
  File "-", line 2, characters 72-77:
  Error: ?id: optional parameters are not allowed in expressions
  [1]
