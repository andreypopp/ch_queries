basic window functions:
  $ ./compile_and_run '
  > let users = [%q "SELECT count(1)over(partition by users.x order by users.x) FROM public.users"];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users =
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
            Ch_queries.Expr.count
              ~order_by:
                (List.concat
                   [
                     [
                       ( Ch_queries.A_expr (__q#users#query (fun users -> users#x)),
                         `ASC );
                     ];
                   ])
              ~partition_by:
                (List.concat
                   [
                     [
                       Ch_queries.A_expr (__q#users#query (fun users -> users#x));
                     ];
                   ])
              (Ch_queries.int 1)
        end)
  >>> RUNNING
  val users :
    < _1 : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr >
    Ch_queries.scope Ch_queries.select

window functions / param in PARTITION BY:
  $ ./compile_and_run '
  > let users ~g = [%q "SELECT count(1)over(partition by ?g... order by users.x) FROM public.users"];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users ~g =
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
            Ch_queries.Expr.count
              ~order_by:
                (List.concat
                   [
                     [
                       ( Ch_queries.A_expr (__q#users#query (fun users -> users#x)),
                         `ASC );
                     ];
                   ])
              ~partition_by:(List.concat [ g __q ])
              (Ch_queries.int 1)
        end)
  >>> RUNNING
  val users :
    g:(< users : < id : (Ch_queries.non_null, int Ch_queries.number)
                        Ch_queries.expr;
                   is_active : (Ch_queries.non_null, bool) Ch_queries.expr;
                   x : (Ch_queries.non_null, string) Ch_queries.expr;
                   xs : (Ch_queries.non_null,
                         (Ch_queries.non_null, string) Ch_queries.array)
                        Ch_queries.expr >
                 Ch_queries.scope > ->
       Ch_queries.a_expr list) ->
    < _1 : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr >
    Ch_queries.scope Ch_queries.select

window functions / param in ORDER BY:
  $ ./compile_and_run '
  > let users ~o = [%q "SELECT count(1)over(partition by 1 order by ?o...) FROM public.users"];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users ~o =
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
            Ch_queries.Expr.count
              ~order_by:(List.concat [ o __q ])
              ~partition_by:
                (List.concat [ [ Ch_queries.A_expr (Ch_queries.int 1) ] ])
              (Ch_queries.int 1)
        end)
  >>> RUNNING
  val users :
    o:(< users : < id : (Ch_queries.non_null, int Ch_queries.number)
                        Ch_queries.expr;
                   is_active : (Ch_queries.non_null, bool) Ch_queries.expr;
                   x : (Ch_queries.non_null, string) Ch_queries.expr;
                   xs : (Ch_queries.non_null,
                         (Ch_queries.non_null, string) Ch_queries.array)
                        Ch_queries.expr >
                 Ch_queries.scope > ->
       (Ch_queries.a_expr * [ `ASC | `DESC ]) list) ->
    < _1 : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr >
    Ch_queries.scope Ch_queries.select
