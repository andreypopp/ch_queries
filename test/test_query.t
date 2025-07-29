basic form:
  $ ./compile_and_run '
  > let users = [%query "SELECT users.x AS x FROM public.users WHERE users.is_active"];;
  > let sql, _parse_row = Queries.query users @@ fun users -> Queries.Row.string [%expr "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users"))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~where:(fun (users : _ Queries.scope) ->
        users#query (fun users -> users#is_active))
  
  let sql, _parse_row =
    Queries.query users @@ fun users ->
    Queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users.x AS _1 FROM public.users AS users WHERE users.is_active) AS q

select from a subquery:
  $ ./compile_and_run '
  > let users = [%query "SELECT q.x AS x FROM (SELECT users.x AS x, users.is_active AS is_active FROM public.users) AS q WHERE q.is_active"];;
  > let sql, _parse_row = Queries.(query users @@ fun users -> Row.string [%expr "users.x"])
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Queries.select ()
      ~from:
        (Queries.from
           (Queries.from_select
              (Queries.select ()
                 ~from:(Queries.from (Database.Public.users ~alias:"users"))
                 ~select:(fun (users : _ Queries.scope) ->
                   object
                     method x = users#query (fun users -> users#x)
                     method is_active = users#query (fun users -> users#is_active)
                   end))
              ~alias:"q"))
      ~select:(fun (q : _ Queries.scope) ->
        object
          method x = q#query (fun q -> q#x)
        end)
      ~where:(fun (q : _ Queries.scope) -> q#query (fun q -> q#is_active))
  
  let sql, _parse_row =
    let open Queries in
    query users @@ fun users -> Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT q._2 AS _1
    FROM (
      SELECT users.is_active AS _1, users.x AS _2 FROM public.users AS users) AS q
    WHERE q._1) AS q

select from an OCaml value:
  $ ./compile_and_run '
  > let users t = [%query "SELECT q.x FROM ?t AS q WHERE q.is_active"]
  > let (_ : _ Queries.select) = users Database.Public.users
  > '
  >>> PREPROCESSING
  let users t =
    Queries.select ()
      ~from:(Queries.from (t ~alias:"q"))
      ~select:(fun (q : _ Queries.scope) ->
        object
          method _1 = q#query (fun q -> q#x)
        end)
      ~where:(fun (q : _ Queries.scope) -> q#query (fun q -> q#is_active))
  
  let (_ : _ Queries.select) = users Database.Public.users
  >>> RUNNING

splicing ocaml values into WHERE:
  $ ./compile_and_run '
  > let users ~where = [%query "SELECT users.x AS x FROM public.users WHERE ?where"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users ~where =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users"))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~where:(fun (users : _ Queries.scope) -> where users)
  >>> RUNNING
  val users :
    where:(< id : (Queries.non_null, int Queries.number) Queries.expr;
             is_active : (Queries.non_null, bool) Queries.expr;
             x : (Queries.non_null, string) Queries.expr >
           Queries.scope -> ('a, bool) Queries.expr) ->
    < x : (Queries.non_null, string) Queries.expr > Queries.scope
    Queries.select

splicing ocaml values into SELECT:
  $ ./compile_and_run '
  > let users ~what = [%query "SELECT ?what AS field FROM public.users"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users ~what =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users"))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method field = what users
        end)
  >>> RUNNING
  val users :
    what:(< id : (Queries.non_null, int Queries.number) Queries.expr;
            is_active : (Queries.non_null, bool) Queries.expr;
            x : (Queries.non_null, string) Queries.expr >
          Queries.scope -> 'a) ->
    < field : 'a > Queries.scope Queries.select

splicing ocaml values into SELECT as scope:
  $ ./compile_and_run '
  > let users ~what = [%query "SELECT ?what... FROM public.users"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users ~what =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users"))
      ~select:what
  >>> RUNNING
  val users :
    what:(< id : (Queries.non_null, int Queries.number) Queries.expr;
            is_active : (Queries.non_null, bool) Queries.expr;
            x : (Queries.non_null, string) Queries.expr >
          Queries.scope -> 'a) ->
    'a Queries.scope Queries.select
