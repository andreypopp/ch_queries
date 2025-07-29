module Database = struct
  open Queries

  module Public = struct
    let users =
      let scope ~alias =
        object
          method id : (non_null, int number) expr = unsafe_expr (alias ^ ".id")
          method x : (non_null, string) expr = unsafe_expr (alias ^ ".x")

          method is_active : (non_null, bool) expr =
            unsafe_expr (alias ^ ".is_active")
        end
      in
      from_table ~db:"public" ~table:"users" scope

    let profiles =
      let scope ~alias =
        object
          method user_id : (non_null, int number) expr =
            unsafe_expr (alias ^ ".user_id")

          method name : (non_null, string) expr = unsafe_expr (alias ^ ".name")
        end
      in
      from_table ~db:"public" ~table:"profiles" scope
  end
end

[@@@ocaml.warning "-27"]

let ( && ) x y = {%expr|?x AND ?y|}

let users ~condition =
  {%query|SELECT u.x AS x, u.id AS id FROM public.users as u WHERE ?condition|}

let x =
  let users =
    users ~condition:(fun u -> {%expr|u.is_active OR true|})
    |> Queries.from_select
  in
  {%query|SELECT u.x AS name, p.name as pname
          FROM ?users as u
          LEFT JOIN public.profiles as p
          ON u.id = p.user_id and p.name = 'Alice'
          WHERE p.name = toNullable('Alice') AND toNullable(false)|}

(** we define a type for metrics we can compute, metrics are parametrized by
    ocaml type and by sql type. *)
type ('t, 'sqlt) metric =
  | Metric_count : (int, int Queries.number) metric
  | Metric_sum_id : (int, int Queries.number) metric
  | Metric_true : (bool, bool) metric

type 'f stats =
  < metric :
      'a 'sqlt. ('a, 'sqlt) metric -> (Queries.non_null, 'sqlt) Queries.expr
  ; .. >
  as
  'f
(** this is a helper type alias to a scope type which can compute metric
    expressions. *)

(** this function selects a metric from a table scope *)
let select_metric : type t sqlt.
    _ Queries.scope -> (t, sqlt) metric -> (_, sqlt) Queries.expr =
 fun users -> function
  | Metric_count -> {%expr|count(1)|}
  | Metric_sum_id -> {%expr|sum(users.id)|}
  | Metric_true -> {%expr|true|}

(** this function merges metric state into final metric value. *)
let merge_metric : type a sqlt.
    _ stats Queries.scope -> (a, sqlt) metric -> (_, sqlt) Queries.expr =
 fun stats m -> stats#query (fun stats -> stats#metric m)

(** this function defines how to query/parse a metric from a query. *)
let query_metric : type t sqlt.
    _ stats Queries.scope -> (t, sqlt) metric -> t Queries.Row.t =
 fun stats m ->
  let open Queries.Row in
  let metric m = stats#query (fun stats -> stats#metric m) in
  match m with
  | Metric_count -> int (metric m)
  | Metric_sum_id -> int (metric m)
  | Metric_true -> bool (metric m)

let users_stats =
  let stats =
    let select (users : _ Queries.scope) : _ stats =
      object
        method metric : type t sqlt. (t, sqlt) metric -> (_, sqlt) Queries.expr
            =
          select_metric users
      end
    in
    {%query|SELECT ?select... FROM public.users|} |> Queries.from_select
  in
  let select (stats : _ stats Queries.scope) : _ stats =
    object
      method oops = {%expr|toNullable('hello')|}

      method metric : type a sqlt. (a, sqlt) metric -> (_, sqlt) Queries.expr =
        merge_metric stats
    end
  in
  let having (stats : _ stats Queries.scope) =
    let is_true = stats#query @@ fun stats -> stats#metric Metric_true in
    {%expr|?is_true|}
  in
  {%query|SELECT ?select... FROM ?stats AS stats HAVING ?having|}

let sql, parse_row =
  Queries.query users_stats (fun (stats : _ stats Queries.scope) ->
      let open Queries.Row in
      let+ x = query_metric stats Metric_count
      and+ y = query_metric stats Metric_true
      and+ z = query_metric stats Metric_sum_id
      and+ s = string_opt {%expr|stats.oops|} in
      (x, y, z, s))
