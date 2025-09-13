basic form:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x FROM public.users WHERE users.is_active"];;
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
               method x = __q#users#query (fun __q -> __q#x)
             end))
      ~select:(fun __q ->
        object
          method x = __q#x
        end)
      ~where:(fun __q -> __q#users#query (fun __q -> __q#is_active))
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users.x AS _1 FROM public.users AS users WHERE users.is_active) AS q

select from a subquery:
  $ ./compile_and_run '
  > let users = [%q "SELECT q.x AS x FROM (SELECT users.x AS x, users.is_active AS is_active FROM public.users) AS q WHERE q.is_active"];;
  > let sql, _parse_row = Ch_queries.(query users @@ fun __q -> Row.ignore [%e "q.x"])
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from
              (Ch_queries.from_select
                 (Ch_queries.select ()
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
                             method x = __q#users#query (fun __q -> __q#x)
  
                             method is_active =
                               __q#users#query (fun __q -> __q#is_active)
                           end))
                    ~select:(fun __q ->
                      object
                        method x = __q#x
                        method is_active = __q#is_active
                      end))
                 ~alias:"q"))
           (fun (q : _ Ch_queries.scope) ->
             let __q =
               object
                 method q = q
               end
             in
             object
               method q = q
               method x = __q#q#query (fun __q -> __q#x)
             end))
      ~select:(fun __q ->
        object
          method x = __q#x
        end)
      ~where:(fun __q -> __q#q#query (fun __q -> __q#is_active))
  
  let sql, _parse_row =
    let open Ch_queries in
    query users @@ fun __q -> Row.ignore (__q#q#query (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT q._2 AS _1
    FROM (
      SELECT users.is_active AS _1, users.x AS _2 FROM public.users AS users) AS q
    WHERE q._1) AS q

select from a subquery (no alias default to "q"):
  $ ./compile_and_run '
  > let users = [%q "SELECT q.x AS x FROM (SELECT users.x AS x, users.is_active AS is_active FROM public.users) WHERE q.is_active"];;
  > let sql, _parse_row = Ch_queries.(query users @@ fun __q -> Row.ignore [%e "q.x"])
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from
              (Ch_queries.from_select
                 (Ch_queries.select ()
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
                             method x = __q#users#query (fun __q -> __q#x)
  
                             method is_active =
                               __q#users#query (fun __q -> __q#is_active)
                           end))
                    ~select:(fun __q ->
                      object
                        method x = __q#x
                        method is_active = __q#is_active
                      end))
                 ~alias:"q"))
           (fun (q : _ Ch_queries.scope) ->
             let __q =
               object
                 method q = q
               end
             in
             object
               method q = q
               method x = __q#q#query (fun __q -> __q#x)
             end))
      ~select:(fun __q ->
        object
          method x = __q#x
        end)
      ~where:(fun __q -> __q#q#query (fun __q -> __q#is_active))
  
  let sql, _parse_row =
    let open Ch_queries in
    query users @@ fun __q -> Row.ignore (__q#q#query (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT q._2 AS _1
    FROM (
      SELECT users.is_active AS _1, users.x AS _2 FROM public.users AS users) AS q
    WHERE q._1) AS q

select from an OCaml value (parameter syntax):
  $ ./compile_and_run '
  > let users t = [%q "SELECT q.x FROM $t AS q WHERE q.is_active"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users t =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from (t ~alias:"q"))
           (fun (q : _ Ch_queries.scope) ->
             object
               method q = q
             end))
      ~select:(fun __q ->
        object
          method x = __q#q#query (fun __q -> __q#x)
        end)
      ~where:(fun __q -> __q#q#query (fun __q -> __q#is_active))
  >>> RUNNING
  val users :
    (alias:string ->
     < is_active : ('a, bool) Ch_queries.expr; x : ('b, 'c) Ch_queries.expr;
       .. >
     Ch_queries.scope Ch_queries.from_one) ->
    < x : ('b, 'c) Ch_queries.expr > Ch_queries.scope Ch_queries.select

select from an OCaml value (id syntax):
  $ ./compile_and_run '
  > let users t = [%q "SELECT t.x FROM t WHERE t.is_active"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users t =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from (t ~alias:"t"))
           (fun (t : _ Ch_queries.scope) ->
             object
               method t = t
             end))
      ~select:(fun __q ->
        object
          method x = __q#t#query (fun __q -> __q#x)
        end)
      ~where:(fun __q -> __q#t#query (fun __q -> __q#is_active))
  >>> RUNNING
  val users :
    (alias:string ->
     < is_active : ('a, bool) Ch_queries.expr; x : ('b, 'c) Ch_queries.expr;
       .. >
     Ch_queries.scope Ch_queries.from_one) ->
    < x : ('b, 'c) Ch_queries.expr > Ch_queries.scope Ch_queries.select

splicing ocaml values into WHERE:
  $ ./compile_and_run '
  > let users ~where = [%q "SELECT users.x AS x FROM public.users WHERE $where"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users ~where =
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
               method x = __q#users#query (fun __q -> __q#x)
             end))
      ~select:(fun __q ->
        object
          method x = __q#x
        end)
      ~where:(fun __q -> where __q)
  >>> RUNNING
  val users :
    where:(< users : Ch_database.Public.users Ch_queries.scope;
             x : (Ch_queries.non_null, string) Ch_queries.expr > ->
           ('a, bool) Ch_queries.expr) ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

splicing ocaml values into WHERE:
  $ ./compile_and_run '
  > let users ~where = [%q "SELECT users.x AS x FROM public.users WHERE $where::Bool"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users ~where =
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
               method x = __q#users#query (fun __q -> __q#x)
             end))
      ~select:(fun __q ->
        object
          method x = __q#x
        end)
      ~where:(fun __q ->
        (where __q : (Ch_queries.non_null, bool) Ch_queries.expr))
  >>> RUNNING
  val users :
    where:(< users : Ch_database.Public.users Ch_queries.scope;
             x : (Ch_queries.non_null, string) Ch_queries.expr > ->
           (Ch_queries.non_null, bool) Ch_queries.expr) ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

splicing ocaml values into SELECT:
  $ ./compile_and_run '
  > let users ~what = [%q "SELECT $what AS field FROM public.users"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users ~what =
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
               method field = what __q
             end))
      ~select:(fun __q ->
        object
          method field = __q#field
        end)
  >>> RUNNING
  val users :
    what:(< users : Ch_database.Public.users Ch_queries.scope > -> 'a) ->
    < field : 'a > Ch_queries.scope Ch_queries.select

splicing ocaml values into SELECT as scope:
  $ ./compile_and_run '
  > let users ~what = [%q "SELECT $what... FROM public.users"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users ~what =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from
              (Ch_database.Public.users ~alias:"users" ~final:false))
           (fun (users : _ Ch_queries.scope) ->
             object
               method users = users
             end))
      ~select:what
  >>> RUNNING
  val users :
    what:(< users : Ch_database.Public.users Ch_queries.scope > -> 'a) ->
    'a Ch_queries.scope Ch_queries.select

select with PREWHERE clause:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x FROM public.users PREWHERE users.is_active WHERE users.id = 10"];;
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
               method x = __q#users#query (fun __q -> __q#x)
             end))
      ~select:(fun __q ->
        object
          method x = __q#x
        end)
      ~prewhere:(fun __q -> __q#users#query (fun __q -> __q#is_active))
      ~where:(fun __q ->
        Ch_queries.Expr.( = )
          (__q#users#query (fun __q -> __q#id))
          (Ch_queries.int 10))
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query (fun __q -> __q#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users.x AS _1
    FROM public.users AS users
    PREWHERE users.is_active
    WHERE users.id = 10) AS q

expressions referenced multiple times result in a single column added to teh subquery:
  $ ./compile_and_run '
  > let users = [%q "SELECT q.is_active AS is_active FROM (SELECT users.is_active AS is_active FROM public.users) AS q WHERE q.is_active"];;
  > let sql, _parse_row = Ch_queries.(query users @@ fun __q -> Row.ignore [%e "q.is_active"])
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from
              (Ch_queries.from_select
                 (Ch_queries.select ()
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
  
                             method is_active =
                               __q#users#query (fun __q -> __q#is_active)
                           end))
                    ~select:(fun __q ->
                      object
                        method is_active = __q#is_active
                      end))
                 ~alias:"q"))
           (fun (q : _ Ch_queries.scope) ->
             let __q =
               object
                 method q = q
               end
             in
             object
               method q = q
               method is_active = __q#q#query (fun __q -> __q#is_active)
             end))
      ~select:(fun __q ->
        object
          method is_active = __q#is_active
        end)
      ~where:(fun __q -> __q#q#query (fun __q -> __q#is_active))
  
  let sql, _parse_row =
    let open Ch_queries in
    query users @@ fun __q -> Row.ignore (__q#q#query (fun __q -> __q#is_active))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT q._1 AS _1
    FROM (SELECT users.is_active AS _1 FROM public.users AS users) AS q
    WHERE q._1) AS q
