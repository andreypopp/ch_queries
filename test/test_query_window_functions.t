basic window functions:
  $ ./compile_and_run '
  > let users = [%query "SELECT count(1)over(partition by users.x order by users.x) FROM public.users"];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method _1 =
            Queries.Expr.count
              ~order_by:
                (List.concat
                   [
                     [
                       (Queries.A_expr (users#query (fun users -> users#x)), `ASC);
                     ];
                   ])
              ~partition_by:
                (List.concat
                   [ [ Queries.A_expr (users#query (fun users -> users#x)) ] ])
              (Queries.int 1)
        end)
  >>> RUNNING
  val users :
    < _1 : (Queries.non_null, int Queries.number) Queries.expr > Queries.scope
    Queries.select

window functions / param in PARTITION BY:
  $ ./compile_and_run '
  > let users ~g = [%query "SELECT count(1)over(partition by ?g... order by users.x) FROM public.users"];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users ~g =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method _1 =
            Queries.Expr.count
              ~order_by:
                (List.concat
                   [
                     [
                       (Queries.A_expr (users#query (fun users -> users#x)), `ASC);
                     ];
                   ])
              ~partition_by:(List.concat [ g users ])
              (Queries.int 1)
        end)
  >>> RUNNING
  val users :
    g:(< id : (Queries.non_null, int Queries.number) Queries.expr;
         is_active : (Queries.non_null, bool) Queries.expr;
         x : (Queries.non_null, string) Queries.expr;
         xs : (Queries.non_null, (Queries.non_null, string) Queries.array)
              Queries.expr >
       Queries.scope -> Queries.a_expr list) ->
    < _1 : (Queries.non_null, int Queries.number) Queries.expr > Queries.scope
    Queries.select

window functions / param in ORDER BY:
  $ ./compile_and_run '
  > let users ~o = [%query "SELECT count(1)over(partition by 1 order by ?o...) FROM public.users"];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users ~o =
    Queries.select ()
      ~from:(Queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method _1 =
            Queries.Expr.count
              ~order_by:(List.concat [ o users ])
              ~partition_by:(List.concat [ [ Queries.A_expr (Queries.int 1) ] ])
              (Queries.int 1)
        end)
  >>> RUNNING
  val users :
    o:(< id : (Queries.non_null, int Queries.number) Queries.expr;
         is_active : (Queries.non_null, bool) Queries.expr;
         x : (Queries.non_null, string) Queries.expr;
         xs : (Queries.non_null, (Queries.non_null, string) Queries.array)
              Queries.expr >
       Queries.scope -> (Queries.a_expr * [ `ASC | `DESC ]) list) ->
    < _1 : (Queries.non_null, int Queries.number) Queries.expr > Queries.scope
    Queries.select
