basic form:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x FROM public.users WHERE users.is_active"];;
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
        users#query (fun users -> users#is_active))
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun users ->
    Ch_queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users.x AS _1 FROM public.users AS users WHERE users.is_active) AS q

unqualified columns are resolved if possible:
  $ ./compile_and_run '
  > let users = [%q "SELECT x AS x FROM public.users WHERE is_active"];;
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
        users#query (fun users -> users#is_active))
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun users ->
    Ch_queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users.x AS _1 FROM public.users AS users WHERE users.is_active) AS q

otherwise, an error is raised:
  $ ./compile_and_run '
  > let users = [%q "SELECT x AS x FROM public.users JOIN public.users AS u2 ON users.id = u2.id"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun users -> Ch_queries.Row.string [%e "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  File "-", line 2, characters 24-25:
  Error: ambiguous column reference
  >>> RUNNING
  File "./test_query.ml", line 8, characters 24-25:
  Error: ambiguous column reference
  [2]

select from a subquery:
  $ ./compile_and_run '
  > let users = [%q "SELECT q.x AS x FROM (SELECT users.x AS x, users.is_active AS is_active FROM public.users) AS q WHERE q.is_active"];;
  > let sql, _parse_row = Ch_queries.(query users @@ fun users -> Row.string [%e "users.x"])
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.from
           (Ch_queries.from_select
              (Ch_queries.select ()
                 ~from:
                   (Ch_queries.from
                      (Database.Public.users ~alias:"users" ~final:false))
                 ~select:(fun (users : _ Ch_queries.scope) ->
                   object
                     method x = users#query (fun users -> users#x)
                     method is_active = users#query (fun users -> users#is_active)
                   end))
              ~alias:"q"))
      ~select:(fun (q : _ Ch_queries.scope) ->
        object
          method x = q#query (fun q -> q#x)
        end)
      ~where:(fun (q : _ Ch_queries.scope) -> q#query (fun q -> q#is_active))
  
  let sql, _parse_row =
    let open Ch_queries in
    query users @@ fun users -> Row.string (users#query (fun users -> users#x))
  
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
  > let sql, _parse_row = Ch_queries.(query users @@ fun users -> Row.string [%e "users.x"])
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.from
           (Ch_queries.from_select
              (Ch_queries.select ()
                 ~from:
                   (Ch_queries.from
                      (Database.Public.users ~alias:"users" ~final:false))
                 ~select:(fun (users : _ Ch_queries.scope) ->
                   object
                     method x = users#query (fun users -> users#x)
                     method is_active = users#query (fun users -> users#is_active)
                   end))
              ~alias:"q"))
      ~select:(fun (q : _ Ch_queries.scope) ->
        object
          method x = q#query (fun q -> q#x)
        end)
      ~where:(fun (q : _ Ch_queries.scope) -> q#query (fun q -> q#is_active))
  
  let sql, _parse_row =
    let open Ch_queries in
    query users @@ fun users -> Row.string (users#query (fun users -> users#x))
  
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
  > let users t = [%q "SELECT q.x FROM ?t AS q WHERE q.is_active"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users t =
    Ch_queries.select ()
      ~from:(Ch_queries.from (t ~alias:"q"))
      ~select:(fun (q : _ Ch_queries.scope) ->
        object
          method x = q#query (fun q -> q#x)
        end)
      ~where:(fun (q : _ Ch_queries.scope) -> q#query (fun q -> q#is_active))
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
      ~from:(Ch_queries.from (t ~alias:"t"))
      ~select:(fun (t : _ Ch_queries.scope) ->
        object
          method x = t#query (fun t -> t#x)
        end)
      ~where:(fun (t : _ Ch_queries.scope) -> t#query (fun t -> t#is_active))
  >>> RUNNING
  val users :
    (alias:string ->
     < is_active : ('a, bool) Ch_queries.expr; x : ('b, 'c) Ch_queries.expr;
       .. >
     Ch_queries.scope Ch_queries.from_one) ->
    < x : ('b, 'c) Ch_queries.expr > Ch_queries.scope Ch_queries.select

splicing ocaml values into WHERE:
  $ ./compile_and_run '
  > let users ~where = [%q "SELECT users.x AS x FROM public.users WHERE ?where"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users ~where =
    Ch_queries.select ()
      ~from:(Ch_queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~where:(fun (users : _ Ch_queries.scope) -> where users)
  >>> RUNNING
  val users :
    where:(< id : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr;
             is_active : (Ch_queries.non_null, bool) Ch_queries.expr;
             x : (Ch_queries.non_null, string) Ch_queries.expr;
             xs : (Ch_queries.non_null,
                   (Ch_queries.non_null, string) Ch_queries.array)
                  Ch_queries.expr >
           Ch_queries.scope -> ('a, bool) Ch_queries.expr) ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

splicing ocaml values into WHERE:
  $ ./compile_and_run '
  > let users ~where = [%q "SELECT users.x AS x FROM public.users WHERE ?where:Bool"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users ~where =
    Ch_queries.select ()
      ~from:(Ch_queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~where:(fun (users : _ Ch_queries.scope) ->
        (where users : (Ch_queries.non_null, bool) Ch_queries.expr))
  >>> RUNNING
  val users :
    where:(< id : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr;
             is_active : (Ch_queries.non_null, bool) Ch_queries.expr;
             x : (Ch_queries.non_null, string) Ch_queries.expr;
             xs : (Ch_queries.non_null,
                   (Ch_queries.non_null, string) Ch_queries.array)
                  Ch_queries.expr >
           Ch_queries.scope -> (Ch_queries.non_null, bool) Ch_queries.expr) ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

splicing ocaml values into SELECT:
  $ ./compile_and_run '
  > let users ~what = [%q "SELECT ?what AS field FROM public.users"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users ~what =
    Ch_queries.select ()
      ~from:(Ch_queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method field = what users
        end)
  >>> RUNNING
  val users :
    what:(< id : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr;
            is_active : (Ch_queries.non_null, bool) Ch_queries.expr;
            x : (Ch_queries.non_null, string) Ch_queries.expr;
            xs : (Ch_queries.non_null,
                  (Ch_queries.non_null, string) Ch_queries.array)
                 Ch_queries.expr >
          Ch_queries.scope -> 'a) ->
    < field : 'a > Ch_queries.scope Ch_queries.select

splicing ocaml values into SELECT as scope:
  $ ./compile_and_run '
  > let users ~what = [%q "SELECT ?what... FROM public.users"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users ~what =
    Ch_queries.select ()
      ~from:(Ch_queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:what
  >>> RUNNING
  val users :
    what:(< id : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr;
            is_active : (Ch_queries.non_null, bool) Ch_queries.expr;
            x : (Ch_queries.non_null, string) Ch_queries.expr;
            xs : (Ch_queries.non_null,
                  (Ch_queries.non_null, string) Ch_queries.array)
                 Ch_queries.expr >
          Ch_queries.scope -> 'a) ->
    'a Ch_queries.scope Ch_queries.select

select from table with FINAL keyword:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x FROM public.users AS users FINAL WHERE users.is_active"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun users -> Ch_queries.Row.string [%e "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:(Ch_queries.from (Database.Public.users ~alias:"users" ~final:true))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~where:(fun (users : _ Ch_queries.scope) ->
        users#query (fun users -> users#is_active))
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun users ->
    Ch_queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users.x AS _1 FROM public.users AS users FINAL WHERE users.is_active)
    AS q

select with PREWHERE clause:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x AS x FROM public.users PREWHERE users.is_active WHERE users.id = 10"];;
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
      ~prewhere:(fun (users : _ Ch_queries.scope) ->
        users#query (fun users -> users#is_active))
      ~where:(fun (users : _ Ch_queries.scope) ->
        Ch_queries.Expr.( = )
          (users#query (fun users -> users#id))
          (Ch_queries.int 10))
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun users ->
    Ch_queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users.x AS _1
    FROM public.users AS users
    PREWHERE users.is_active
    WHERE (users.id = 10)) AS q

expressions referenced multiple times result in a single column added to teh subquery:
  $ ./compile_and_run '
  > let users = [%q "SELECT q.is_active AS is_active FROM (SELECT users.is_active AS is_active FROM public.users) AS q WHERE q.is_active"];;
  > let sql, _parse_row = Ch_queries.(query users @@ fun users -> Row.bool [%e "users.is_active"])
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.from
           (Ch_queries.from_select
              (Ch_queries.select ()
                 ~from:
                   (Ch_queries.from
                      (Database.Public.users ~alias:"users" ~final:false))
                 ~select:(fun (users : _ Ch_queries.scope) ->
                   object
                     method is_active = users#query (fun users -> users#is_active)
                   end))
              ~alias:"q"))
      ~select:(fun (q : _ Ch_queries.scope) ->
        object
          method is_active = q#query (fun q -> q#is_active)
        end)
      ~where:(fun (q : _ Ch_queries.scope) -> q#query (fun q -> q#is_active))
  
  let sql, _parse_row =
    let open Ch_queries in
    query users @@ fun users ->
    Row.bool (users#query (fun users -> users#is_active))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT q._1 AS _1
    FROM (SELECT users.is_active AS _1 FROM public.users AS users) AS q
    WHERE q._1) AS q
