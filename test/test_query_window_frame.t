ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING:

  $ ./compile_and_run "
  > let e = {%e|count(1) over(partition by 1 order by 2 rows between unbounded preceding and unbounded following)|};;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  count(1) OVER (PARTITION BY 1 ORDER BY 2 ASC ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING)

RANGE BETWEEN:

  $ ./compile_and_run "
  > let e = {%e|count(1) over(partition by 1 order by 2 range between unbounded preceding and current row)|};;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  count(1) OVER (PARTITION BY 1 ORDER BY 2 ASC RANGE BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)

ROWS BETWEEN offset PRECEDING AND offset FOLLOWING:

  $ ./compile_and_run "
  > let e = {%e|sum(1) over(order by 2 rows between 3 preceding and 5 following)|};;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  sum(1) OVER (ORDER BY 2 ASC ROWS BETWEEN 3 PRECEDING AND 5 FOLLOWING)

Single-bound ROWS frame (without BETWEEN):

  $ ./compile_and_run "
  > let e = {%e|count(1) over(order by 2 rows unbounded preceding)|};;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  count(1) OVER (ORDER BY 2 ASC ROWS UNBOUNDED PRECEDING)

Frame without PARTITION BY or ORDER BY:

  $ ./compile_and_run "
  > let e = {%e|count(1) over(rows between unbounded preceding and current row)|};;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  count(1) OVER (ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)

PPX generates frame argument:

  $ ./compile_and_run '
  > let users = [%q "SELECT count(1)over(partition by users.x order by users.x rows between unbounded preceding and unbounded following) FROM public.users"];;
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
              ~frame:(`ROWS (`UNBOUNDED_PRECEDING, Some `UNBOUNDED_FOLLOWING))
              ~order_by:
                (List.concat
                   [
                     [
                       ( Ch_queries.A_expr
                           (__q#users#query ?alias:(Some "x") (fun __q -> __q#x)),
                         `ASC );
                     ];
                   ])
              ~partition_by:
                (List.concat
                   [
                     [
                       Ch_queries.A_expr
                         (__q#users#query ?alias:(Some "x") (fun __q -> __q#x));
                     ];
                   ])
              (Ch_queries.int 1)
        end)
  >>> RUNNING
  val users :
    < _1 : (Ch_queries.non_null, int64 Ch_queries.number) Ch_queries.expr >
    Ch_queries.scope Ch_queries.select

Window frame with GROUP BY:

  $ ./compile_and_run "
  > let users = [%q {q|SELECT users.x AS x, sum(users.id)over(partition by users.x order by users.id rows between unbounded preceding and current row) AS s FROM public.users GROUP BY users.x|q}];;
  > let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e {e|q.s|e}];;
  > let () = print_endline sql;;
  > "
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
               method x = __q#users#query ?alias:(Some "x") (fun __q -> __q#x)
  
               method s =
                 Ch_queries.Expr.sum
                   ~frame:(`ROWS (`UNBOUNDED_PRECEDING, Some `CURRENT_ROW))
                   ~order_by:
                     (List.concat
                        [
                          [
                            ( Ch_queries.A_expr
                                (__q#users#query ?alias:(Some "id") (fun __q ->
                                     __q#id)),
                              `ASC );
                          ];
                        ])
                   ~partition_by:
                     (List.concat
                        [
                          [
                            Ch_queries.A_expr
                              (__q#users#query ?alias:(Some "x") (fun __q ->
                                   __q#x));
                          ];
                        ])
                   (__q#users#query ?alias:(Some "id") (fun __q -> __q#id))
             end))
      ~select:(fun __q ->
        object
          method x = __q#x
          method s = __q#s
        end)
      ~group_by:(fun __q ->
        List.concat
          [
            [
              Ch_queries.A_expr
                (__q#users#query ?alias:(Some "x") (fun __q -> __q#x));
            ];
          ])
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun __q ->
    Ch_queries.Row.ignore (__q#q#query ?alias:(Some "s") (fun __q -> __q#s))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT
    sum(users.id) OVER (PARTITION BY users.x ORDER BY users.id ASC ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
      AS s
  FROM public.users AS users
  GROUP BY users.x

range() still works as a function despite RANGE keyword:

  $ ./compile_and_run "
  > let e = {%e|range(10)|};;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  range(10)
