select from a JOIN:
  $ ./compile_and_run '
  > let q = [%query "SELECT u.id, p.name FROM public.users AS u JOIN public.profiles AS p ON u.id = p.user_id WHERE u.is_active"]
  > '
  >>> PREPROCESSING
  let q =
    Queries.select ()
      ~from:
        (Queries.join
           (Queries.from (Database.Public.users ~alias:"u" ~final:false))
           (Database.Public.profiles ~alias:"p" ~final:false)
           ~on:(fun ((u : _ Queries.scope), (p : _ Queries.scope)) ->
             Queries.Expr.( = )
               (u#query (fun u -> u#id))
               (p#query (fun p -> p#user_id))))
      ~select:(fun ((u : _ Queries.scope), (p : _ Queries.scope)) ->
        object
          method _1 = u#query (fun u -> u#id)
          method _2 = p#query (fun p -> p#name)
        end)
      ~where:(fun ((u : _ Queries.scope), (p : _ Queries.scope)) ->
        u#query (fun u -> u#is_active))
  >>> RUNNING

select from a LEFT JOIN:
  $ ./compile_and_run '
  > let q = [%query "
  >   SELECT u.id AS user_id, p.name AS user_name
  >   FROM public.users AS u
  >   LEFT JOIN public.profiles AS p
  >   ON u.id = p.user_id
  > "];;
  > #show q
  > '
  >>> PREPROCESSING
  let q =
    Queries.select ()
      ~from:
        (Queries.left_join
           (Queries.from (Database.Public.users ~alias:"u" ~final:false))
           (Database.Public.profiles ~alias:"p" ~final:false)
           ~on:(fun ((u : _ Queries.scope), (p : _ Queries.scope)) ->
             Queries.Expr.( = )
               (u#query (fun u -> u#id))
               (p#query (fun p -> p#user_id))))
      ~select:(fun ((u : _ Queries.scope), (p : _ Queries.nullable_scope)) ->
        object
          method user_id = u#query (fun u -> u#id)
          method user_name = p#query (fun p -> p#name)
        end)
  >>> RUNNING
  val q :
    < user_id : (Queries.non_null, int Queries.number) Queries.expr;
      user_name : (Queries.null, string) Queries.expr >
    Queries.scope Queries.select

select from an OCaml value with JOIN:
  $ ./compile_and_run '
  > let q profiles = [%query "
  >   SELECT u.id AS user_id, p.name AS user_name
  >   FROM public.users AS u
  >   LEFT JOIN ?profiles AS p
  >   ON u.id = p.user_id
  > "];;
  > #show q
  > let q = q (Database.Public.profiles ~final:false);;
  > #show q
  > '
  >>> PREPROCESSING
  let q profiles =
    Queries.select ()
      ~from:
        (Queries.left_join
           (Queries.from (Database.Public.users ~alias:"u" ~final:false))
           (profiles ~alias:"p")
           ~on:(fun ((u : _ Queries.scope), (p : _ Queries.scope)) ->
             Queries.Expr.( = )
               (u#query (fun u -> u#id))
               (p#query (fun p -> p#user_id))))
      ~select:(fun ((u : _ Queries.scope), (p : _ Queries.nullable_scope)) ->
        object
          method user_id = u#query (fun u -> u#id)
          method user_name = p#query (fun p -> p#name)
        end)
  
  let q = q (Database.Public.profiles ~final:false)
  >>> RUNNING
  val q :
    (alias:string ->
     < name : ('a, 'b) Queries.expr;
       user_id : (Queries.non_null, int Queries.number) Queries.expr; .. >
     Queries.scope Queries.from_one) ->
    < user_id : (Queries.non_null, int Queries.number) Queries.expr;
      user_name : (Queries.null, 'b) Queries.expr >
    Queries.scope Queries.select
  val q :
    < user_id : (Queries.non_null, int Queries.number) Queries.expr;
      user_name : (Queries.null, string) Queries.expr >
    Queries.scope Queries.select

splicing ocaml values into JOIN-ON:
  $ ./compile_and_run '
  > let q cond = [%query "
  >   SELECT 1 as one
  >   FROM public.users AS u
  >   LEFT JOIN public.profiles AS p ON ?cond
  >   LEFT JOIN public.profiles AS p2 ON true
  > "];;
  > #show q
  > '
  >>> PREPROCESSING
  let q cond =
    Queries.select ()
      ~from:
        (Queries.left_join
           (Queries.left_join
              (Queries.from (Database.Public.users ~alias:"u" ~final:false))
              (Database.Public.profiles ~alias:"p" ~final:false)
              ~on:(fun ((u : _ Queries.scope), (p : _ Queries.scope)) ->
                cond (u, p)))
           (Database.Public.profiles ~alias:"p2" ~final:false)
           ~on:(fun
               ( ((u : _ Queries.scope), (p : _ Queries.nullable_scope)),
                 (p2 : _ Queries.scope) )
             -> Queries.bool true))
      ~select:(fun
          ( ((u : _ Queries.scope), (p : _ Queries.nullable_scope)),
            (p2 : _ Queries.nullable_scope) )
        ->
        object
          method one = Queries.int 1
        end)
  >>> RUNNING
  val q :
    (< id : (Queries.non_null, int Queries.number) Queries.expr;
       is_active : (Queries.non_null, bool) Queries.expr;
       x : (Queries.non_null, string) Queries.expr;
       xs : (Queries.non_null, (Queries.non_null, string) Queries.array)
            Queries.expr >
     Queries.scope *
     < name : (Queries.non_null, string) Queries.expr;
       user_id : (Queries.non_null, int Queries.number) Queries.expr >
     Queries.scope -> ('a, bool) Queries.expr) ->
    < one : (Queries.non_null, int Queries.number) Queries.expr > Queries.scope
    Queries.select
