select from a JOIN:
  $ ./compile_and_run '
  > let q = [%q "SELECT u.id, p.name FROM public.users AS u JOIN public.profiles AS p ON u.id = p.user_id WHERE u.is_active"]
  > '
  >>> PREPROCESSING
  let q =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.join
              (Ch_queries.from (Ch_database.Public.users ~alias:"u" ~final:false))
              (Ch_database.Public.profiles ~alias:"p" ~final:false)
              ~on:(fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.scope)) ->
                let __q =
                  object
                    method u = u
                    method p = p
                  end
                in
                Ch_queries.Expr.( = )
                  (__q#u#query (fun __q -> __q#id))
                  (__q#p#query (fun __q -> __q#user_id))))
           (fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.scope)) ->
             object
               method u = u
               method p = p
             end))
      ~select:(fun __q ->
        object
          method id = __q#u#query (fun __q -> __q#id)
          method name = __q#p#query (fun __q -> __q#name)
        end)
      ~where:(fun __q -> __q#u#query (fun __q -> __q#is_active))
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
        (Ch_queries.map_from_scope
           (Ch_queries.left_join
              (Ch_queries.from (Ch_database.Public.users ~alias:"u" ~final:false))
              (Ch_database.Public.profiles ~alias:"p" ~final:false)
              ~on:(fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.scope)) ->
                let __q =
                  object
                    method u = u
                    method p = p
                  end
                in
                Ch_queries.Expr.( = )
                  (__q#u#query (fun __q -> __q#id))
                  (__q#p#query (fun __q -> __q#user_id))))
           (fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.nullable_scope)) ->
             let __q =
               object
                 method u = u
                 method p = p
               end
             in
             object
               method u = u
               method p = p
               method user_id = __q#u#query (fun __q -> __q#id)
               method user_name = __q#p#query (fun __q -> __q#name)
             end))
      ~select:(fun __q ->
        object
          method user_id = __q#user_id
          method user_name = __q#user_name
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
  >   LEFT JOIN $profiles AS p
  >   ON u.id = p.user_id
  > "];;
  > #show q
  > let q = q (Ch_database.Public.profiles ~final:false);;
  > #show q
  > '
  >>> PREPROCESSING
  let q profiles =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.left_join
              (Ch_queries.from (Ch_database.Public.users ~alias:"u" ~final:false))
              (profiles ~alias:"p")
              ~on:(fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.scope)) ->
                let __q =
                  object
                    method u = u
                    method p = p
                  end
                in
                Ch_queries.Expr.( = )
                  (__q#u#query (fun __q -> __q#id))
                  (__q#p#query (fun __q -> __q#user_id))))
           (fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.nullable_scope)) ->
             let __q =
               object
                 method u = u
                 method p = p
               end
             in
             object
               method u = u
               method p = p
               method user_id = __q#u#query (fun __q -> __q#id)
               method user_name = __q#p#query (fun __q -> __q#name)
             end))
      ~select:(fun __q ->
        object
          method user_id = __q#user_id
          method user_name = __q#user_name
        end)
  
  let q = q (Ch_database.Public.profiles ~final:false)
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
  >   LEFT JOIN public.profiles AS p ON $cond
  >   LEFT JOIN public.profiles AS p2 ON true
  > "];;
  > #show q
  > '
  >>> PREPROCESSING
  let q cond =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.left_join
              (Ch_queries.left_join
                 (Ch_queries.from
                    (Ch_database.Public.users ~alias:"u" ~final:false))
                 (Ch_database.Public.profiles ~alias:"p" ~final:false)
                 ~on:(fun ((u : _ Ch_queries.scope), (p : _ Ch_queries.scope)) ->
                   let __q =
                     object
                       method u = u
                       method p = p
                     end
                   in
                   cond __q))
              (Ch_database.Public.profiles ~alias:"p2" ~final:false)
              ~on:(fun
                  ( ((u : _ Ch_queries.scope), (p : _ Ch_queries.nullable_scope)),
                    (p2 : _ Ch_queries.scope) )
                ->
                let __q =
                  object
                    method u = u
                    method p = p
                    method p2 = p2
                  end
                in
                Ch_queries.bool true))
           (fun ( ((u : _ Ch_queries.scope), (p : _ Ch_queries.nullable_scope)),
                  (p2 : _ Ch_queries.nullable_scope) )
              ->
             let __q =
               object
                 method u = u
                 method p = p
                 method p2 = p2
               end
             in
             object
               method u = u
               method p = p
               method p2 = p2
               method one = Ch_queries.int 1
             end))
      ~select:(fun __q ->
        object
          method one = __q#one
        end)
  >>> RUNNING
  val q :
    (< p : Ch_database.Public.profiles Ch_queries.scope;
       u : Ch_database.Public.users Ch_queries.scope > ->
     ('a, bool) Ch_queries.expr) ->
    < one : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr >
    Ch_queries.scope Ch_queries.select
