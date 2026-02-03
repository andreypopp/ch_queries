uniqMerge basic:
  $ ./compile_and_run '
  > let q = [%q "SELECT uniqMerge(users.uniq_state) FROM public.users"];;
  > #show q;;
  > '
  >>> PREPROCESSING
  let q =
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
          method _1 =
            Ch_queries.Expr.uniqMerge
              (__q#users#query (fun __q -> __q#uniq_state))
        end)
  >>> RUNNING
  val q :
    < _1 : (Ch_queries.non_null, int64 Ch_queries.number) Ch_queries.expr >
    Ch_queries.scope Ch_queries.select

uniqMerge with window:
  $ ./compile_and_run '
  > let q = [%q "SELECT uniqMerge(users.uniq_state)over(partition by users.id) FROM public.users"];;
  > #show q;;
  > '
  >>> PREPROCESSING
  let q =
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
          method _1 =
            Ch_queries.Expr.uniqMerge
              ~partition_by:
                (List.concat
                   [ [ Ch_queries.A_expr (__q#users#query (fun __q -> __q#id)) ] ])
              (__q#users#query (fun __q -> __q#uniq_state))
        end)
  >>> RUNNING
  val q :
    < _1 : (Ch_queries.non_null, int64 Ch_queries.number) Ch_queries.expr >
    Ch_queries.scope Ch_queries.select

uniqMergeState basic:
  $ ./compile_and_run '
  > let q = [%q "SELECT uniqMergeState(users.uniq_state) FROM public.users"];;
  > #show q;;
  > '
  >>> PREPROCESSING
  let q =
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
          method _1 =
            Ch_queries.Expr.uniqMergeState
              (__q#users#query (fun __q -> __q#uniq_state))
        end)
  >>> RUNNING
  val q :
    < _1 : (Ch_queries.non_null,
            (Ch_queries.non_null, int64 Ch_queries.number) Ch_queries.agg_state)
           Ch_queries.expr >
    Ch_queries.scope Ch_queries.select

uniqMergeState with window:
  $ ./compile_and_run '
  > let q = [%q "SELECT uniqMergeState(users.uniq_state)over(partition by users.id order by users.x) FROM public.users"];;
  > #show q;;
  > '
  >>> PREPROCESSING
  let q =
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
          method _1 =
            Ch_queries.Expr.uniqMergeState
              ~order_by:
                (List.concat
                   [
                     [
                       ( Ch_queries.A_expr (__q#users#query (fun __q -> __q#x)),
                         `ASC );
                     ];
                   ])
              ~partition_by:
                (List.concat
                   [ [ Ch_queries.A_expr (__q#users#query (fun __q -> __q#id)) ] ])
              (__q#users#query (fun __q -> __q#uniq_state))
        end)
  >>> RUNNING
  val q :
    < _1 : (Ch_queries.non_null,
            (Ch_queries.non_null, int64 Ch_queries.number) Ch_queries.agg_state)
           Ch_queries.expr >
    Ch_queries.scope Ch_queries.select
