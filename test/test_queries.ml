module Database = struct
  open Queries

  module Public = struct
    let users =
      let scope ~alias =
        object
          method id : (non_null, int number) expr = unsafe (alias ^ ".id")
          method x : (non_null, string) expr = unsafe (alias ^ ".x")

          method xs : (non_null, (non_null, string) array) expr =
            unsafe (alias ^ ".xs")

          method is_active : (non_null, bool) expr =
            unsafe (alias ^ ".is_active")
        end
      in
      from_table ~db:"public" ~table:"users" scope

    let profiles =
      let scope ~alias =
        object
          method user_id : (non_null, int number) expr =
            unsafe (alias ^ ".user_id")

          method name : (non_null, string) expr = unsafe (alias ^ ".name")
        end
      in
      from_table ~db:"public" ~table:"profiles" scope
  end
end

[@@@ocaml.warning "-27"]

let ( && ) x y = {%e|?x AND ?y|}

let users ~condition =
  {%q|SELECT u.x AS x, u.id AS id FROM public.users as u WHERE ?condition|}

let x =
  let users =
    users ~condition:(fun u -> {%e|u.is_active OR true|}) |> Queries.from_select
  in
  {%q|SELECT u.x AS name, p.name as pname
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
  | Metric_count -> {%e|count(1)|}
  | Metric_sum_id -> {%e|sum(users.id)|}
  | Metric_true -> {%e|true|}

(** this function merges metric state into final metric value. *)
let merge_metric : type a sqlt.
    _ stats Queries.scope -> (a, sqlt) metric -> (_, sqlt) Queries.expr =
 fun stats m -> {%e|stats.metric(?{m})|}

(** this function defines how to query/parse a metric from a query. *)
let query_metric : type t sqlt.
    _ stats Queries.scope -> (t, sqlt) metric -> t Queries.Row.t =
 fun stats m ->
  let open Queries.Row in
  let metric m = {%e|stats.metric(?{m})|} in
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
    {%q|SELECT ?select... FROM public.users|} |> Queries.from_select
  in
  let select (stats : _ stats Queries.scope) : _ stats =
    object
      method oops = {%e|toNullable('hello')|}

      method metric : type a sqlt. (a, sqlt) metric -> (_, sqlt) Queries.expr =
        merge_metric stats
    end
  in
  let having (stats : _ stats Queries.scope) =
    let is_true = stats#query @@ fun stats -> stats#metric Metric_true in
    {%e|?is_true|}
  in
  {%q|SELECT ?select... FROM ?stats AS stats HAVING ?having|}

let sql, parse_row =
  Queries.query users_stats (fun (stats : _ stats Queries.scope) ->
      let open Queries.Row in
      let+ x = query_metric stats Metric_count
      and+ y = query_metric stats Metric_true
      and+ z = query_metric stats Metric_sum_id
      and+ z' = bool @@ {%e|stats.metric(?{Metric_true})|}
      and+ s = string_opt {%e|stats.oops|} in
      (x, y, z, s))
