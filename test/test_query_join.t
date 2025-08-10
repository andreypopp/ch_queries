select from a JOIN:
  $ ./compile_and_run '
  > let q = [%q "SELECT u.id, p.name FROM public.users AS u JOIN public.profiles AS p ON u.id = p.user_id WHERE u.is_active"]
  > '
  >>> PREPROCESSING
  let q =
    Ch_queries.select ()
      ~from:
        (Ch_queries.join
           (Ch_queries.from (Database.Public.users ~alias:"u" ~final:false))
           (Database.Public.profiles ~alias:"p" ~final:false)
           ~on:(fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.scope)) ->
             Ch_queries.Expr.( = )
               (u#query (fun u -> u#id))
               (p#query (fun p -> p#user_id))))
      ~select:(fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.scope)) ->
        object
          method id = u#query (fun u -> u#id)
          method name = p#query (fun p -> p#name)
        end)
      ~where:(fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.scope)) ->
        u#query (fun u -> u#is_active))
  >>> RUNNING

select from a LEFT JOIN:
  $ ./compile_and_run '
  > let q = [%q "
  >   SELECT u.id AS user_id, p.name AS user_name
  >   FROM public.users AS u
  >   LEFT JOIN public.profiles AS p
  >   ON u.id = p.user_id
  > "];;
  > #show q
  > '
  >>> PREPROCESSING
  let q =
    Ch_queries.select ()
      ~from:
        (Ch_queries.left_join
           (Ch_queries.from (Database.Public.users ~alias:"u" ~final:false))
           (Database.Public.profiles ~alias:"p" ~final:false)
           ~on:(fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.scope)) ->
             Ch_queries.Expr.( = )
               (u#query (fun u -> u#id))
               (p#query (fun p -> p#user_id))))
      ~select:(fun
          ((u : _ Ch_queries.scope), (p : _ Ch_queries.nullable_scope)) ->
        object
          method user_id = u#query (fun u -> u#id)
          method user_name = p#query (fun p -> p#name)
        end)
  >>> RUNNING
  val q :
    < user_id : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr;
      user_name : (Ch_queries.null, string) Ch_queries.expr >
    Ch_queries.scope Ch_queries.select

select from an OCaml value with JOIN:
  $ ./compile_and_run '
  > let q profiles = [%q "
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
    Ch_queries.select ()
      ~from:
        (Ch_queries.left_join
           (Ch_queries.from (Database.Public.users ~alias:"u" ~final:false))
           (profiles ~alias:"p")
           ~on:(fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.scope)) ->
             Ch_queries.Expr.( = )
               (u#query (fun u -> u#id))
               (p#query (fun p -> p#user_id))))
      ~select:(fun
          ((u : _ Ch_queries.scope), (p : _ Ch_queries.nullable_scope)) ->
        object
          method user_id = u#query (fun u -> u#id)
          method user_name = p#query (fun p -> p#name)
        end)
  
  let q = q (Database.Public.profiles ~final:false)
  >>> RUNNING
  val q :
    (alias:string ->
     < name : ('a, 'b) Ch_queries.expr;
       user_id : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr;
       .. >
     Ch_queries.scope Ch_queries.from_one) ->
    < user_id : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr;
      user_name : (Ch_queries.null, 'b) Ch_queries.expr >
    Ch_queries.scope Ch_queries.select
  val q :
    < user_id : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr;
      user_name : (Ch_queries.null, string) Ch_queries.expr >
    Ch_queries.scope Ch_queries.select

splicing ocaml values into JOIN-ON:
  $ ./compile_and_run '
  > let q cond = [%q "
  >   SELECT 1 as one
  >   FROM public.users AS u
  >   LEFT JOIN public.profiles AS p ON ?cond
  >   LEFT JOIN public.profiles AS p2 ON true
  > "];;
  > #show q
  > '
  >>> PREPROCESSING
  let q cond =
    Ch_queries.select ()
      ~from:
        (Ch_queries.left_join
           (Ch_queries.left_join
              (Ch_queries.from (Database.Public.users ~alias:"u" ~final:false))
              (Database.Public.profiles ~alias:"p" ~final:false)
              ~on:(fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.scope)) ->
                cond (u, p)))
           (Database.Public.profiles ~alias:"p2" ~final:false)
           ~on:(fun
               ( ((u : _ Ch_queries.scope), (p : _ Ch_queries.nullable_scope)),
                 (p2 : _ Ch_queries.scope) )
             -> Ch_queries.bool true))
      ~select:(fun
          ( ((u : _ Ch_queries.scope), (p : _ Ch_queries.nullable_scope)),
            (p2 : _ Ch_queries.nullable_scope) )
        ->
        object
          method one = Ch_queries.int 1
        end)
  >>> RUNNING
  val q :
    (< id : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr;
       is_active : (Ch_queries.non_null, bool) Ch_queries.expr;
       x : (Ch_queries.non_null, string) Ch_queries.expr;
       xs : (Ch_queries.non_null,
             (Ch_queries.non_null, string) Ch_queries.array)
            Ch_queries.expr >
     Ch_queries.scope *
     < name : (Ch_queries.non_null, string) Ch_queries.expr;
       user_id : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr >
     Ch_queries.scope -> ('a, bool) Ch_queries.expr) ->
    < one : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr >
    Ch_queries.scope Ch_queries.select
