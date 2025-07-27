
  $ dotest() {
  > rm -f test_query.ml
  > echo '
  > #use "topfind"
  > #require "queries"
  > #require "queries.ppx"
  > #use "test_queries.ml"
  > ' > test_query.ml
  > echo '>>> PREPROCESSING'
  > echo "$1" | rg -v '^#' | ppx_queries -impl -
  > echo '>>> RUNNING'
  > echo "$1" >> test_query.ml
  > ocaml ./test_query.ml
  > }

basic form:
  $ dotest '
  > let users = [%query "SELECT users.x AS x FROM public.users WHERE users.is_active"]
  > '
  >>> PREPROCESSING
  let users =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users"))
      ~select:(fun (users : _ Queries.scope) ->
                 object method x = users#query (fun users -> users#x) end)
      ~where:(fun (users : _ Queries.scope) ->
                users#query (fun users -> users#is_active))
  >>> RUNNING

select from a subquery:
  $ dotest '
  > let users = [%query "SELECT q.x FROM (SELECT users.x AS x FROM public.users) AS q"]
  > '
  >>> PREPROCESSING
  let users =
    Queries.select ()
      ~from:(Queries.from
               ((Queries.select ()
                   ~from:(Queries.from (Database.Public.users ~alias:"users"))
                   ~select:(fun (users : _ Queries.scope) ->
                              object
                                method x = users#query (fun users -> users#x)
                              end)) ~alias:"q"))
      ~select:(fun (q : _ Queries.scope) ->
                 object method _1 = q#query (fun q -> q#x) end)
  >>> RUNNING

select from a JOIN:
  $ dotest '
  > let q = [%query "SELECT u.id, p.name FROM public.users AS u JOIN public.profiles AS p ON u.id = p.user_id WHERE u.is_active"]
  > '
  >>> PREPROCESSING
  let q =
    Queries.select ()
      ~from:(Queries.join (Queries.from (Database.Public.users ~alias:"u"))
               (Database.Public.profiles ~alias:"p")
               ~on:(fun
                      (((u : _ Queries.scope) : _ Queries.scope),
                       (p : _ Queries.scope))
                      ->
                      Queries.Expr.(=) (u#query (fun u -> u#id))
                        (p#query (fun p -> p#user_id))))
      ~select:(fun
                 (((u : _ Queries.scope) : _ Queries.scope),
                  (p : _ Queries.scope))
                 ->
                 object
                   method _1 = u#query (fun u -> u#id)
                   method _2 = p#query (fun p -> p#name)
                 end)
      ~where:(fun
                (((u : _ Queries.scope) : _ Queries.scope),
                 (p : _ Queries.scope))
                -> u#query (fun u -> u#is_active))
  >>> RUNNING

select from a JOIN:
  $ dotest '
  > let q = [%query "
  >   SELECT u.id AS user_id, p.name AS user_name
  >   FROM public.users AS u
  >   LEFT JOIN public.profiles AS p
  >   ON u.id = p.user_id
  >   WHERE coalesce(p.user_id, 0) = 1
  > "];;
  > #show q
  > '
  >>> PREPROCESSING
  let q =
    Queries.select ()
      ~from:(Queries.left_join
               (Queries.from (Database.Public.users ~alias:"u"))
               (Database.Public.profiles ~alias:"p")
               ~on:(fun
                      (((u : _ Queries.scope) : _ Queries.scope),
                       (p : _ Queries.scope))
                      ->
                      Queries.Expr.(=) (u#query (fun u -> u#id))
                        (p#query (fun p -> p#user_id))))
      ~select:(fun
                 (((u : _ Queries.scope) : _ Queries.scope),
                  (p : _ Queries.nullable_scope))
                 ->
                 object
                   method user_id = u#query (fun u -> u#id)
                   method user_name = p#query (fun p -> p#name)
                 end)
      ~where:(fun
                (((u : _ Queries.scope) : _ Queries.scope),
                 (p : _ Queries.nullable_scope))
                ->
                Queries.Expr.(=)
                  (Queries.Expr.coalesce (p#query (fun p -> p#user_id))
                     (Queries.Expr.int 0)) (Queries.Expr.int 1))
  >>> RUNNING
  val q :
    alias:string ->
    < user_id : (Queries.Expr.non_null, int Queries.Expr.number) Queries.Expr.t;
      user_name : (Queries.Expr.null, string) Queries.Expr.t >
    Queries.scope Queries.from_one

select from an OCaml value:
  $ dotest '
  > let users t = [%query "SELECT q.x FROM t AS q WHERE q.is_active"]
  > let (_ : alias:string -> _ Queries.from_one) = users Database.Public.users
  > '
  >>> PREPROCESSING
  let users t =
    Queries.select () ~from:(Queries.from (t ~alias:"q"))
      ~select:(fun (q : _ Queries.scope) ->
                 object method _1 = q#query (fun q -> q#x) end)
      ~where:(fun (q : _ Queries.scope) -> q#query (fun q -> q#is_active))
  let (_ : alias:string -> _ Queries.from_one) = users Database.Public.users
  >>> RUNNING

select from an OCaml value with JOIN:
  $ dotest '
  > let q profiles = [%query "
  >   SELECT u.id AS user_id, p.name AS user_name
  >   FROM public.users AS u
  >   LEFT JOIN profiles AS p
  >   ON u.id = p.user_id
  >   WHERE coalesce(p.user_id, 0) = 1
  > "];;
  > #show q
  > let q = q Database.Public.profiles;;
  > #show q
  > '
  >>> PREPROCESSING
  let q profiles =
    Queries.select ()
      ~from:(Queries.left_join
               (Queries.from (Database.Public.users ~alias:"u"))
               (profiles ~alias:"p")
               ~on:(fun
                      (((u : _ Queries.scope) : _ Queries.scope),
                       (p : _ Queries.scope))
                      ->
                      Queries.Expr.(=) (u#query (fun u -> u#id))
                        (p#query (fun p -> p#user_id))))
      ~select:(fun
                 (((u : _ Queries.scope) : _ Queries.scope),
                  (p : _ Queries.nullable_scope))
                 ->
                 object
                   method user_id = u#query (fun u -> u#id)
                   method user_name = p#query (fun p -> p#name)
                 end)
      ~where:(fun
                (((u : _ Queries.scope) : _ Queries.scope),
                 (p : _ Queries.nullable_scope))
                ->
                Queries.Expr.(=)
                  (Queries.Expr.coalesce (p#query (fun p -> p#user_id))
                     (Queries.Expr.int 0)) (Queries.Expr.int 1))
  let q = q Database.Public.profiles
  >>> RUNNING
  val q :
    (alias:string ->
     < name : ('a, 'b) Queries.Expr.t;
       user_id : (Queries.Expr.non_null, int Queries.Expr.number)
                 Queries.Expr.t;
       .. >
     Queries.scope Queries.from_one) ->
    alias:string ->
    < user_id : (Queries.Expr.non_null, int Queries.Expr.number) Queries.Expr.t;
      user_name : (Queries.Expr.null, 'b) Queries.Expr.t >
    Queries.scope Queries.from_one
  val q :
    alias:string ->
    < user_id : (Queries.Expr.non_null, int Queries.Expr.number) Queries.Expr.t;
      user_name : (Queries.Expr.null, string) Queries.Expr.t >
    Queries.scope Queries.from_one
