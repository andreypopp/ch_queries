module Ch_database = struct
  open Ch_queries

  module Public = struct
    type users =
      < id : (non_null, int number) expr
      ; is_active : (non_null, bool) expr
      ; x : (non_null, string) expr
      ; xs : (non_null, (non_null, string) Ch_queries.array) expr
      ; uniq_state : (non_null, (non_null, int64 number) agg_state) expr >

    let users : final:bool -> alias:string -> users scope from_one =
      let scope ~alias =
        object
          method id : (non_null, int number) expr = unsafe_col alias "id"
          method x : (non_null, string) expr = unsafe_col alias "x"

          method xs : (non_null, (non_null, string) array) expr =
            unsafe_col alias "xs"

          method is_active : (non_null, bool) expr =
            unsafe_col alias "is_active"

          method uniq_state :
              (non_null, (non_null, int64 number) agg_state) expr =
            unsafe_col alias "uniq_state"
        end
      in
      from_table ~db:"public" ~table:"users" scope

    type profiles =
      < name : (non_null, string) expr ; user_id : (non_null, int number) expr >

    let profiles : final:bool -> alias:string -> profiles scope from_one =
      let scope ~alias =
        object
          method user_id : (non_null, int number) expr =
            unsafe_col alias "user_id"

          method name : (non_null, string) expr = unsafe_col alias "name"
        end
      in
      from_table ~db:"public" ~table:"profiles" scope

    let dict =
      Dict.make ~db:"public" ~table:"dict"
        ~keys:(unsafe "key" : (non_null, int number) expr)
        ~values:
          (object
             method value : (non_null, string) expr = Dict.unsafe_value "value"
          end)

    let multikey_dict =
      Dict.make ~db:"public" ~table:"dict"
        ~keys:
          (Expr.tuple2
             ( (unsafe "key" : (non_null, int number) expr),
               (unsafe "key" : (non_null, int number) expr) ))
        ~values:
          (object
             method value : (non_null, string) expr = Dict.unsafe_value "value"
          end)
  end
end

[@@@ocaml.warning "-27"]

let ( && ) x y = {%e|$x AND $y|}

let users ~condition =
  {%q|SELECT
        u.x AS x,
        u.id AS id
      FROM public.users as u WHERE $.condition|}

let x =
  let users =
    users ~condition:(fun {%s|...|} -> {%e|u.is_active OR true|})
    |> Ch_queries.from_select
  in
  let select {%s|u ..., p public.profiles?, ...|} =
    object
      method name = {%e|u.x|}
      method pname = {%e|p.name|}
      method u = __q#u
      method p = __q#p
    end
  in
  {%q|SELECT $.select...
      FROM users as u
      LEFT JOIN public.profiles as p
      ON u.id = p.user_id and p.name = 'Alice'
      WHERE p.name = toNullable('Alice') AND toNullable(false)|}

let y =
  let users = x |> Ch_queries.from_select in
  Ch_queries.select () ~from:{%f|FROM $users AS users|}
    ~select:(fun {%s|...|} ->
      object
        method select = {%e|users.(coalesce(p.name, u.x))|}
      end)
    ~where:(fun {%s|...|} -> {%e|users.name = 'Alive'|})

(** we define a type for metrics we can compute, metrics are parametrized by
    ocaml type and by sql type. *)
type ('t, 'sqlt) metric =
  | Metric_count : (int64, int64 Ch_queries.number) metric
  | Metric_sum_id : (int, int Ch_queries.number) metric
  | Metric_true : (bool, bool) metric

type 'f stats =
  < metric :
      'a 'sqlt.
      ('a, 'sqlt) metric -> (Ch_queries.non_null, 'sqlt) Ch_queries.expr
  ; .. >
  as
  'f
(** this is a helper type alias to a scope type which can compute metric
    expressions. *)

(** this function selects a metric from a table scope *)
let select_metric : type t sqlt.
    {%s|users public.users|} -> (t, sqlt) metric -> (_, sqlt) Ch_queries.expr =
 fun {%s|...|} -> function
  | Metric_count -> {%e|count(1)|}
  | Metric_sum_id -> {%e|sum(users.id)|}
  | Metric_true -> {%e|true|}

(** this function merges metric state into final metric value. *)
let merge_metric : type a sqlt.
    < stats : _ stats Ch_queries.scope > ->
    (a, sqlt) metric ->
    (_, sqlt) Ch_queries.expr =
 fun {%s|...|} (m : (a, sqlt) metric) -> {%e|stats.metric(${m})|}

(** this function defines how to query/parse a metric from a query. *)
let query_metric : type t sqlt.
    < q : _ stats Ch_queries.scope > -> (t, sqlt) metric -> t Ch_queries.Row.t =
 fun {%s|...|} m ->
  let open Ch_queries in
  let metric m = {%e|q.metric(${m})|} in
  match m with
  | Metric_count -> Row.col (metric m) Parse.int64
  | Metric_sum_id -> Row.col (metric m) Parse.int
  | Metric_true -> Row.col (metric m) Parse.bool

let users_stats =
  let stats =
    let select {%s|...|} : _ stats =
      object
        method metric : type t sqlt.
            (t, sqlt) metric -> (_, sqlt) Ch_queries.expr =
          select_metric __q
      end
    in
    {%q|SELECT $.select... FROM public.users|} |> Ch_queries.from_select
  in
  let select (__q : < stats : _ stats Ch_queries.scope >) : _ stats =
    object
      method oops = {%e|toNullable('hello')|}

      method metric : type a sqlt. (a, sqlt) metric -> (_, sqlt) Ch_queries.expr
          =
        merge_metric __q
    end
  in
  let having (__q : < stats : _ stats Ch_queries.scope >) =
    let is_true = __q#stats#query @@ fun stats -> stats#metric Metric_true in
    {%e|$is_true|}
  in
  {%q|SELECT $.select... FROM stats HAVING $.having|}

let sql, parse_row =
  Ch_queries.query users_stats (fun __q ->
      let open Ch_queries in
      let open Row in
      let+ x = query_metric __q Metric_count
      and+ y = query_metric __q Metric_true
      and+ z = query_metric __q Metric_sum_id
      and+ z' = Row.col {%e|q.metric(${Metric_true})|} Parse.bool
      and+ s = Row.col {%e|q.oops|} Parse.(nullable string) in
      (x, y, z, s))
