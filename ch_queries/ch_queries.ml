module Syntax = Ch_queries_syntax.Syntax
module SS = Set.Make (String)

let fail fmt = Printf.ksprintf failwith fmt

type uint64 = Unsigned.uint64
type null = [ `null | `not_null ]
type non_null = [ `not_null ]
type 'a nullable = [< null ] as 'a
type 'a comparable = private A_comparable
type 'a number0 = private A_number
type 'a number = 'a number0 comparable
type 'a timestamp0 = private A_timestamp
type 'a timestamp = 'a timestamp0 comparable
type date0 = private Date
type date = date0 timestamp
type datetime0 = private DateTime
type datetime = datetime0 timestamp
type datetime640 = private DateTime64
type datetime64 = datetime640 timestamp
type ('null, 'a) array = private A_array
type ('nullk, 'k, 'nullv, 'v) map = private A_map
type ('null, 'a) typ = private A_typ
type ('x, 'y) tuple2 = private A_tuple2
type ('x, 'y, 'z) tuple3 = private A_tuple3
type ('x, 'y, 'z, 'w) tuple4 = private A_tuple4
type interval = private Interval
type ('n, 'a) agg_state = private Agg_state

type (+'null, +'typ) expr = Syntax.expr

and 'a in_rhs =
  | In_query : < _1 : (_, 'a) expr > scope select -> 'a in_rhs
  | In_array : (_, (_, 'a) array) expr -> 'a in_rhs

and a_expr = A_expr : _ expr -> a_expr

and 'a scope =
  < query' :
      'n 'e.
      ('a -> ('n, 'e) expr) -> ('n, 'e) expr * (force:bool -> ('n, 'e) expr)
  ; query : 'n 'e. ('a -> ('n, 'e) expr) -> ('n, 'e) expr
  ; query_many : ('a -> a_expr list) -> a_expr list >

and 'a nullable_scope =
  < query' :
      'n 'e.
      ('a -> ('n, 'e) expr) -> ('n, 'e) expr * (force:bool -> (null, 'e) expr)
  ; query : 'n 'e. ('a -> ('n, 'e) expr) -> (null, 'e) expr
  ; query_many : ('a -> a_expr list) -> a_expr list >

and a_field = A_field : Ch_queries_syntax.Syntax.expr * string -> a_field

and fill = {
  fill_from : a_expr option;
  fill_to : a_expr option;
  fill_step : a_expr option;
  fill_interpolate : (string * a_expr option) list;
}

and an_order_by = a_expr * [ `ASC | `DESC ] * fill option

and 'scope select_payload = {
  mutable ctes : cte_query list;
  from : a_from0;
  scope : 'scope;  (** scope of the query, once it is queried *)
  prewhere : a_expr option;
  where : a_expr option;
  qualify : a_expr option;
  group_by : a_expr list option;
  having : a_expr option;
  order_by : an_order_by list option;
  limit : a_expr option;
  offset : a_expr option;
  settings :
    (string * [ `Int of int | `String of string | `Bool of bool ]) list;
  mutable rev_fields : a_field list;
      (** list of fields build within the SELECT *)
  mutable fields_aliases : SS.t;
}

and cte_query = Cte_query : string * _ select0 * bool -> cte_query

and 'scope union_payload = {
  x : 'scope select0;
  y : 'scope select0;
  scope : 'scope;
  mutable exprs : a_expr list;  (** list of expressions built within the UNION *)
}

and 'scope select0 =
  | Select of 'scope select_payload
  | Union of 'scope union_payload

and 'scope select = alias:string -> 'scope select0

and 'a from_one0 =
  | From_table : {
      db : string;
      table : string;
      alias : string;
      scope : 'a;
      final : bool;
      mutable used : bool;
    }
      -> 'a from_one0
  | From_select : {
      select : 'a select0;
      alias : string;
      cluster_name : string option;
    }
      -> 'a from_one0
  | From_cte_ref : {
      cte : 'a from_one0;
      cte_alias : string;
      scope : 'a;
      alias : string;
    }
      -> 'a from_one0

and 'a from_one = unit -> 'a from_one0

and 'a from0 =
  | From : { scope : 'a; from : a_from_one0 } -> 'a from0
  | From_join : {
      scope : 'a * 'b;
      kind : [ `INNER_JOIN | `LEFT_JOIN | `LEFT_JOIN_OPTIONAL ];
      from : a_from0;
      join : a_from_one0;
      on : a_expr Lazy.t;
          (** Need this to be lazy so we can support optional joins - we don't
              want to materialize the join if it is not used besides the ON
              clause. *)
    }
      -> ('a * 'b) from0
  | From_map : 'a from0 * ('a -> 'b) -> 'b from0

and 'a from = unit -> 'a from0
and a_from0 = A_from : 'a from0 -> a_from0
and a_from_one0 = A_from_one : 'a from_one0 -> a_from_one0

module Dict = struct
  type ('keys, 'values) t = {
    db : string;
    table : string;
    keys : 'keys;
    values : 'values scope;
  }

  let make ~db ~table ~keys ~values =
    let values =
      object (self)
        method query' f =
          let e = f values in
          let commit ~force:_ = e in
          (e, commit)

        method query f = snd (self#query' f) ~force:false
        method query_many f = f values
      end
    in
    { db; table; keys; values }

  let unsafe_value name = Syntax.make_expr (E_lit (L_string name))
end

let rec scope_from : type scope. scope from0 -> scope = function
  | From { scope; _ } -> scope
  | From_join { scope; _ } -> scope
  | From_map (from, f) -> f (scope_from from)

let from_select ?cluster_name ~alias select () =
  From_select { select = select ~alias; alias; cluster_name }

let from_table ~db ~table scope =
 fun ~final ~alias () : _ scope from_one0 ->
  let scope = scope ~alias in
  let rec t =
    lazy
      (From_table
         {
           db;
           table;
           alias;
           scope =
             (object (self)
                method query' f =
                  let e = f scope in
                  let commit ~force:_ =
                    let () =
                      match Lazy.force t with
                      | From_table t -> t.used <- true
                      | _ -> ()
                    in
                    e
                  in
                  (e, commit)

                method query f = snd (self#query' f) ~force:false

                method query_many f =
                  let () =
                    match Lazy.force t with
                    | From_table t -> t.used <- true
                    | _ -> ()
                  in
                  f scope
              end
               : _ scope);
           final;
           used = false;
         })
  in
  Lazy.force t

let scope_from_select select : _ scope =
  match select with Select { scope; _ } -> scope | Union { scope; _ } -> scope

let scope_from_one = function
  | From_table { scope; _ } -> scope
  | From_select { select; alias = _; cluster_name = _ } ->
      scope_from_select select
  | From_cte_ref { scope; cte = _; cte_alias = _; alias = _ } -> scope

let from from () =
  let from = from () in
  let scope = scope_from_one from in
  From { scope; from = A_from_one from }

let join from join ~on () =
  let from = from () in
  let join = join () in
  let scope_join = scope_from_one join in
  let scope = (scope_from from, scope_join) in
  From_join
    {
      scope;
      kind = `INNER_JOIN;
      from = A_from from;
      join = A_from_one join;
      on = lazy (A_expr (on scope));
    }

let left_join ?(optional = false) from (join : 'a scope from_one) ~on () =
  let from = from () in
  let join = join () in
  let scope_from = scope_from from in
  let scope_join' : _ scope =
    object
      method query' f = (scope_from_one join)#query' f
      method query f = (scope_from_one join)#query f
      method query_many f = (scope_from_one join)#query_many f
    end
  in
  let scope_join : _ nullable_scope =
    object
      method query' f = scope_join'#query' f
      method query f = scope_join'#query f
      method query_many f = scope_join'#query_many f
    end
  in
  From_join
    {
      scope = (scope_from, scope_join);
      kind = (if optional then `LEFT_JOIN_OPTIONAL else `LEFT_JOIN);
      from = A_from from;
      join = A_from_one join;
      on = lazy (A_expr (on (scope_from, scope_join')));
    }

let map_from_scope : type x y. x from -> (x -> y) -> y from =
 fun from f () -> From_map (from (), f)

module To_syntax = struct
  open Ch_queries_syntax

  let rec group_by_to_syntax dimensions =
    List.map dimensions ~f:(fun (A_expr expr) -> Syntax.Dimension_expr expr)

  and fill_to_syntax { fill_from; fill_to; fill_step; fill_interpolate } =
    let fill_from = Option.map (fun (A_expr e) -> e) fill_from in
    let fill_to = Option.map (fun (A_expr e) -> e) fill_to in
    let fill_step = Option.map (fun (A_expr e) -> e) fill_step in
    let fill_interpolate =
      List.map fill_interpolate ~f:(fun (col, expr_opt) ->
          {
            Syntax.interpolate_col = Syntax.make_id col;
            interpolate_expr = Option.map (fun (A_expr e) -> e) expr_opt;
          })
    in
    { Syntax.fill_from; fill_to; fill_step; fill_interpolate }

  and order_by_to_syntax order_by =
    List.map order_by ~f:(fun (A_expr expr, dir, fill) ->
        let fill = Option.map fill_to_syntax fill in
        Syntax.Order_by_expr (expr, dir, fill))

  and to_syntax : type a. a select0 -> Syntax.query =
   fun q ->
    let rec select_to_syntax : type a. a select0 -> Syntax.querysyn = function
      | Select
          {
            ctes;
            from = A_from from;
            prewhere;
            where;
            qualify;
            group_by;
            having;
            order_by;
            limit;
            offset;
            settings;
            rev_fields;
            fields_aliases = _;
            scope = _;
          } ->
          let select =
            List.rev_map
              ~f:(fun (A_field (expr, alias)) ->
                { Syntax.expr; alias = Some (Syntax.make_id alias) })
              rev_fields
          in
          let prewhere = Option.map (fun (A_expr expr) -> expr) prewhere in
          let where = Option.map (fun (A_expr expr) -> expr) where in
          let qualify = Option.map (fun (A_expr expr) -> expr) qualify in
          let group_by = Option.map group_by_to_syntax group_by in
          let having = Option.map (fun (A_expr expr) -> expr) having in
          let order_by = Option.map order_by_to_syntax order_by in
          let limit = Option.map (fun (A_expr expr) -> expr) limit in
          let offset = Option.map (fun (A_expr expr) -> expr) offset in
          let settings =
            List.map settings ~f:(fun (id, value) ->
                let setting_value =
                  match value with
                  | `Int n -> Syntax.Setting_lit (L_int n)
                  | `String s -> Syntax.Setting_lit (L_string s)
                  | `Bool b -> Syntax.Setting_lit (L_bool b)
                in
                Syntax.Setting_item (Syntax.make_id id, setting_value))
          in
          let from = from_to_syntax from in
          let with_fields =
            List.map ctes ~f:(fun (Cte_query (alias, query, materialized)) ->
                let query = query in
                let query = to_syntax query in
                let alias = Syntax.make_id alias in
                Syntax.With_query (alias, query, materialized))
          in
          Q_select
            {
              with_fields;
              select = Select_fields select;
              from;
              prewhere;
              where;
              qualify;
              group_by;
              having;
              order_by;
              limit;
              offset;
              settings;
            }
      | Union { x; y; exprs = _; scope = _ } ->
          Q_union (to_syntax x, to_syntax y)
    and from_one_to_syntax : type a. a from_one0 -> Syntax.from_one =
     fun from ->
      match from with
      | From_table { db; table; alias; final; _ } ->
          Syntax.make_from_one
            (F_table
               {
                 db = Syntax.make_id db;
                 table = Syntax.make_id table;
                 alias = Syntax.make_id alias;
                 final;
               })
      | From_select { select; alias; cluster_name } ->
          Syntax.make_from_one
            (F_select
               {
                 select = Syntax.make_query (select_to_syntax select);
                 alias = Syntax.make_id alias;
                 cluster_name =
                   Option.map
                     (fun name -> Syntax.Cluster_name (Syntax.make_id name))
                     cluster_name;
               })
      | From_cte_ref { cte_alias; alias; _ } ->
          let param = Syntax.make_id cte_alias in
          let alias = Syntax.make_id alias in
          Syntax.make_from_one
            (F_param
               {
                 param = { param; param_has_scope = false };
                 alias;
                 final = false;
               })
    and from_to_syntax : type a. a from0 -> Syntax.from =
     fun from ->
      match from with
      | From_map (from, _f) -> from_to_syntax from
      | From { from = A_from_one from; _ } ->
          let from = from_one_to_syntax from in
          Syntax.make_from (F from)
      | From_join { kind; from = A_from from; join = A_from_one join; on; _ }
        -> (
          let translate () =
            let kind =
              match kind with
              | `INNER_JOIN -> `INNER_JOIN
              | `LEFT_JOIN | `LEFT_JOIN_OPTIONAL -> `LEFT_JOIN
            in
            let (A_expr on) = Lazy.force on in
            let from = from_to_syntax from in
            let join = from_one_to_syntax join in
            Syntax.make_from (F_join { kind; from; join; on })
          in
          match kind with
          | `LEFT_JOIN_OPTIONAL ->
              let rec should_eliminate = function
                | From_table { used; _ } -> not used
                | From_cte_ref { cte; _ } -> should_eliminate cte
                | From_select { select; _ } -> (
                    match select with
                    | Select { rev_fields; _ } -> List.is_empty rev_fields
                    | Union { exprs; _ } -> List.is_empty exprs)
              in
              if not (should_eliminate join) then translate ()
              else from_to_syntax from
          | `LEFT_JOIN | `INNER_JOIN -> translate ())
    in
    Syntax.make_query (select_to_syntax q)
end

let select_syntax ~from ?prewhere ?where ?qualify ?group_by ?having ?order_by
    ?limit ?offset ?(settings = []) ~select () : Ch_queries_syntax.Syntax.query
    =
  let from = from () in
  let inner_scope = scope_from from in
  let prewhere =
    Option.map (fun prewhere -> A_expr (prewhere inner_scope)) prewhere
  in
  let where = Option.map (fun where -> A_expr (where inner_scope)) where in
  let qualify =
    Option.map (fun qualify -> A_expr (qualify inner_scope)) qualify
  in
  let group_by = Option.map (fun group_by -> group_by inner_scope) group_by in
  let having = Option.map (fun having -> A_expr (having inner_scope)) having in
  let order_by = Option.map (fun order_by -> order_by inner_scope) order_by in
  let limit = Option.map (fun limit -> A_expr (limit inner_scope)) limit in
  let offset = Option.map (fun offset -> A_expr (offset inner_scope)) offset in
  let fields_aliases, rev_fields =
    List.fold_left (select inner_scope) ~init:(SS.empty, [])
      ~f:(fun (fields_aliases, rev_fields) (A_field (expr, alias)) ->
        (SS.add alias fields_aliases, A_field (expr, alias) :: rev_fields))
  in
  let select =
    Select
      {
        ctes = [];
        from = A_from from;
        scope = ();
        prewhere;
        where;
        qualify;
        group_by;
        having;
        order_by;
        limit;
        offset;
        settings;
        rev_fields;
        fields_aliases;
      }
  in
  To_syntax.to_syntax select

let expr_to_syntax x = x
let expr_to_string x = Ch_queries_syntax.Printer.print_expr x
let unsafe x = Syntax.make_expr (E_unsafe (Syntax.make_id x))

let unsafe_col q x =
  Syntax.make_expr (E_col (Syntax.make_id q, Syntax.make_id x))

let unsafe_concat xs =
  let xs = List.map xs ~f:(fun (A_expr expr) -> expr) in
  Syntax.make_expr (Syntax.E_unsafe_concat xs)

let lit lit = Syntax.make_expr (Syntax.E_lit lit)
let int x = lit (L_int x)
let string x = lit (L_string x)
let bool x = lit (L_bool x)
let float x = lit (L_float x)
let null = lit L_null
let int64 x = unsafe (Printf.sprintf "toInt64(%s)" (Int64.to_string x))

let uint64 x =
  unsafe (Printf.sprintf "toUInt64(%s)" (Unsigned.UInt64.to_string x))

let date_s x = unsafe (Printf.sprintf "toDate('%s')" x)
let datetime_s x = unsafe (Printf.sprintf "toDateTime('%s')" x)

let date x =
  date_s
    (let t = Unix.gmtime x in
     Printf.sprintf "%04u-%02u-%02u" (1900 + t.Unix.tm_year) (t.Unix.tm_mon + 1)
       t.Unix.tm_mday)

let datetime x =
  datetime_s
    (let t = Unix.gmtime x in
     Printf.sprintf "%04u-%02u-%02uT%02u:%02u:%02u" (1900 + t.Unix.tm_year)
       (t.Unix.tm_mon + 1) t.Unix.tm_mday t.Unix.tm_hour t.Unix.tm_min
       t.Unix.tm_sec)

let interval n unit =
  let unit' =
    match unit with
    | `YEAR -> Syntax.Year
    | `MONTH -> Syntax.Month
    | `WEEK -> Syntax.Week
    | `DAY -> Syntax.Day
    | `HOUR -> Syntax.Hour
    | `MINUTE -> Syntax.Minute
    | `SECOND -> Syntax.Second
  in
  lit (L_interval (n, unit'))

let lambda :
    string ->
    (('pn, 'pa) expr -> ('n, 'a) expr) ->
    (non_null, ('pn, 'pa) expr -> ('n, 'a) expr) expr =
 fun param f ->
  let body = f (unsafe param) in
  Syntax.make_expr (E_lambda ([ Syntax.make_id param ], body))

let lambda2 :
    string ->
    string ->
    (('pn1, 'pa1) expr -> ('pn2, 'pa2) expr -> ('n, 'a) expr) ->
    (non_null, ('pn1, 'pa1) expr -> ('pn2, 'pa2) expr -> ('n, 'a) expr) expr =
 fun param1 param2 f ->
  let body = f (unsafe param1) (unsafe param2) in
  Syntax.make_expr
    (E_lambda ([ Syntax.make_id param1; Syntax.make_id param2 ], body))

let array xs = Syntax.make_expr (E_call (Syntax.Func (Syntax.make_id "["), xs))

module Expr = struct
  let def n args =
    Syntax.make_expr (Syntax.E_call (Syntax.Func (Syntax.make_id n), args))

  (** {1 Regular functions} *)

  (** {2 Arithmetic} *)
  let plus x y = def "+" [ x; y ]

  let ( + ) = plus
  let minus x y = def "-" [ x; y ]
  let ( - ) = minus
  let ( * ) x y = def "*" [ x; y ]
  let multiply x y = def "*" [ x; y ]
  let divide x y = def "/" [ x; y ]
  let ( / ) = divide
  let negate x = def "-" [ x ]
  let abs x = def "abs" [ x ]
  let intDiv x y = def "intDiv" [ x; y ]
  let intDivOrNull x y = def "intDivOrNull" [ x; y ]
  let intDivOrZero x y = def "intDivOrZero" [ x; y ]
  let modulo x y = def "modulo" [ x; y ]
  let moduloOrNull x y = def "moduloOrNull" [ x; y ]
  let moduloOrZero x y = def "moduloOrZero" [ x; y ]
  let positiveModulo x y = def "positiveModulo" [ x; y ]
  let positiveModuloOrNull x y = def "positiveModuloOrNull" [ x; y ]

  let divideDecimal ?result_scale x y =
    match result_scale with
    | None -> def "divideDecimal" [ x; y ]
    | Some scale -> def "divideDecimal" [ x; y; scale ]

  let multiplyDecimal ?result_scale x y =
    match result_scale with
    | None -> def "multiplyDecimal" [ x; y ]
    | Some scale -> def "multiplyDecimal" [ x; y; scale ]

  let divideOrNull x y = def "divideOrNull" [ x; y ]
  let gcd x y = def "gcd" [ x; y ]
  let lcm x y = def "lcm" [ x; y ]
  let max2 x y = def "max2" [ x; y ]
  let min2 x y = def "min2" [ x; y ]
  let midpoint xs = def "midpoint" xs

  (** {2 Arrays} *)

  let arrayElement arr i = def "arrayElement" [ arr; i ]
  let arrayElementOrNull arr i = def "arrayElementOrNull" [ arr; i ]
  let arrayFilter f x = def "arrayFilter" [ f; x ]
  let arrayMap f xs = def "arrayMap" (f :: xs)
  let arrayAll f xs = def "arrayAll" (f :: xs)
  let arrayAvg f xs = def "arrayAvg" (f :: xs)

  let arrayCount ?f xs =
    match f with
    | None -> def "arrayCount" xs
    | Some f -> def "arrayCount" (f :: xs)

  let arrayCumSum ?f xs =
    match f with
    | None -> def "arrayCumSum" xs
    | Some f -> def "arrayCumSum" (f :: xs)

  let arrayCumSumNonNegative ?f xs =
    match f with
    | None -> def "arrayCumSumNonNegative" xs
    | Some f -> def "arrayCumSumNonNegative" (f :: xs)

  let length x = def "length" [ x ]
  let arrayJoin arr = def "arrayJoin" [ arr ]
  let arrayCompact arr = def "arrayCompact" [ arr ]
  let arrayConcat arrs = def "arrayConcat" arrs
  let arrayDifference arr = def "arrayDifference" [ arr ]
  let arrayDistinct arr = def "arrayDistinct" [ arr ]
  let arrayDotProduct v1 v2 = def "arrayDotProduct" [ v1; v2 ]
  let arrayEnumerate arr = def "arrayEnumerate" [ arr ]
  let arrayEnumerateDense arr = def "arrayEnumerateDense" [ arr ]

  let arrayEnumerateDenseRanked clear_depth arr max_array_depth =
    def "arrayEnumerateDenseRanked" [ clear_depth; arr; max_array_depth ]

  let arrayEnumerateUniq arrs = def "arrayEnumerateUniq" arrs

  let arrayEnumerateUniqRanked clear_depth arr max_array_depth =
    def "arrayEnumerateUniqRanked" [ clear_depth; arr; max_array_depth ]

  let arrayExcept source except = def "arrayExcept" [ source; except ]
  let arrayExists f xs = def "arrayExists" (f :: xs)
  let arrayFill f xs = def "arrayFill" (f :: xs)
  let arrayReverseFill f xs = def "arrayReverseFill" (f :: xs)
  let arrayFirst f xs = def "arrayFirst" (f :: xs)
  let arrayFirstIndex f xs = def "arrayFirstIndex" (f :: xs)
  let arrayFirstOrNull f xs = def "arrayFirstOrNull" (f :: xs)
  let arrayFlatten arr = def "arrayFlatten" [ arr ]
  let arrayLast f xs = def "arrayLast" (f :: xs)
  let arrayLastIndex f xs = def "arrayLastIndex" (f :: xs)
  let arrayLastOrNull f xs = def "arrayLastOrNull" (f :: xs)
  let arrayFold f xs acc = def "arrayFold" ((f :: xs) @ [ acc ])
  let arrayIntersect arrs = def "arrayIntersect" arrs
  let arraySymmetricDifference arrs = def "arraySymmetricDifference" arrs
  let arrayUnion arrs = def "arrayUnion" arrs
  let arrayUniq arrs = def "arrayUniq" arrs
  let arrayWithConstant length x = def "arrayWithConstant" [ length; x ]
  let arrayZip arrs = def "arrayZip" arrs
  let arrayZipUnaligned arrs = def "arrayZipUnaligned" arrs
  let arrayJaccardIndex arr_x arr_y = def "arrayJaccardIndex" [ arr_x; arr_y ]

  let arrayLevenshteinDistance arr_from arr_to =
    def "arrayLevenshteinDistance" [ arr_from; arr_to ]

  let arrayLevenshteinDistanceWeighted arr_from arr_to from_weights to_weights =
    def "arrayLevenshteinDistanceWeighted"
      [ arr_from; arr_to; from_weights; to_weights ]

  let arrayMax ?f xs =
    match f with None -> def "arrayMax" xs | Some f -> def "arrayMax" (f :: xs)

  let arrayMin ?f xs =
    match f with None -> def "arrayMin" xs | Some f -> def "arrayMin" (f :: xs)

  let arraySum ?f xs =
    match f with None -> def "arraySum" xs | Some f -> def "arraySum" (f :: xs)

  let arrayPopBack arr = def "arrayPopBack" [ arr ]
  let arrayPopFront arr = def "arrayPopFront" [ arr ]
  let arrayPushBack arr x = def "arrayPushBack" [ arr; x ]
  let arrayPushFront arr x = def "arrayPushFront" [ arr; x ]

  let arrayProduct ?f xs =
    match f with
    | None -> def "arrayProduct" xs
    | Some f -> def "arrayProduct" (f :: xs)

  let arrayPartialReverseSort ?f arrs limit =
    match f with
    | None -> def "arrayPartialReverseSort" (arrs @ [ limit ])
    | Some f -> def "arrayPartialReverseSort" ((f :: arrs) @ [ limit ])

  let arrayPartialSort ?f arrs limit =
    match f with
    | None -> def "arrayPartialSort" (arrs @ [ limit ])
    | Some f -> def "arrayPartialSort" ((f :: arrs) @ [ limit ])

  let arrayPartialShuffle ?limit ?seed arr =
    match (limit, seed) with
    | None, None -> def "arrayPartialShuffle" [ arr ]
    | Some limit, None -> def "arrayPartialShuffle" [ arr; limit ]
    | Some limit, Some seed -> def "arrayPartialShuffle" [ arr; limit; seed ]
    | None, Some _ ->
        failwith "arrayPartialShuffle: seed requires limit to be specified"

  let arrayRandomSample arr samples = def "arrayRandomSample" [ arr; samples ]
  let arrayReduce agg_func arrs = def "arrayReduce" (agg_func :: arrs)

  let arrayReduceInRanges agg_func ranges arrs =
    def "arrayReduceInRanges" (agg_func :: ranges :: arrs)

  let arrayRemove arr elem = def "arrayRemove" [ arr; elem ]

  let arrayResize ?extender arr size =
    match extender with
    | None -> def "arrayResize" [ arr; size ]
    | Some ext -> def "arrayResize" [ arr; size; ext ]

  let arrayReverse arr = def "arrayReverse" [ arr ]
  let arrayRotateLeft arr n = def "arrayRotateLeft" [ arr; n ]
  let arrayRotateRight arr n = def "arrayRotateRight" [ arr; n ]
  let arrayShingles arr l = def "arrayShingles" [ arr; l ]

  let arrayReverseSort ?f arrs =
    match f with
    | None -> def "arrayReverseSort" arrs
    | Some f -> def "arrayReverseSort" (f :: arrs)

  let arraySort ?f arrs =
    match f with
    | None -> def "arraySort" arrs
    | Some f -> def "arraySort" (f :: arrs)

  let arrayReverseSplit f xs = def "arrayReverseSplit" (f :: xs)
  let arraySplit f xs = def "arraySplit" (f :: xs)

  let arrayShiftLeft ?default arr n =
    match default with
    | None -> def "arrayShiftLeft" [ arr; n ]
    | Some d -> def "arrayShiftLeft" [ arr; n; d ]

  let arrayShiftRight ?default arr n =
    match default with
    | None -> def "arrayShiftRight" [ arr; n ]
    | Some d -> def "arrayShiftRight" [ arr; n; d ]

  let arrayShuffle ?seed arr =
    match seed with
    | None -> def "arrayShuffle" [ arr ]
    | Some s -> def "arrayShuffle" [ arr; s ]

  let arraySimilarity arr_from arr_to from_weights to_weights =
    def "arraySimilarity" [ arr_from; arr_to; from_weights; to_weights ]

  let arraySlice ?length arr offset =
    match length with
    | None -> def "arraySlice" [ arr; offset ]
    | Some len -> def "arraySlice" [ arr; offset; len ]

  let countEqual arr x = def "countEqual" [ arr; x ]

  let emptyArrayDate () = def "emptyArrayDate" []
  let emptyArrayDateTime () = def "emptyArrayDateTime" []
  let emptyArrayFloat32 () = def "emptyArrayFloat32" []
  let emptyArrayFloat64 () = def "emptyArrayFloat64" []
  let emptyArrayInt16 () = def "emptyArrayInt16" []
  let emptyArrayInt32 () = def "emptyArrayInt32" []
  let emptyArrayInt64 () = def "emptyArrayInt64" []
  let emptyArrayInt8 () = def "emptyArrayInt8" []
  let emptyArrayString () = def "emptyArrayString" []
  let emptyArrayToSingle arr = def "emptyArrayToSingle" [ arr ]
  let emptyArrayUInt16 () = def "emptyArrayUInt16" []
  let emptyArrayUInt32 () = def "emptyArrayUInt32" []
  let emptyArrayUInt64 () = def "emptyArrayUInt64" []
  let emptyArrayUInt8 () = def "emptyArrayUInt8" []

  let has arr x = def "has" [ arr; x ]
  let hasAll set subset = def "hasAll" [ set; subset ]
  let hasAny arr_x arr_y = def "hasAny" [ arr_x; arr_y ]
  let hasSubstr arr1 arr2 = def "hasSubstr" [ arr1; arr2 ]

  let indexOf arr x = def "indexOf" [ arr; x ]
  let indexOfAssumeSorted arr x = def "indexOfAssumeSorted" [ arr; x ]

  let range ?start ?step end_ =
    match (start, step) with
    | None, None -> def "range" [ end_ ]
    | Some s, None -> def "range" [ s; end_ ]
    | Some s, Some st -> def "range" [ s; end_; st ]
    | None, Some _ -> failwith "range: step requires start to be specified"

  let replicate x arr = def "replicate" [ x; arr ]
  let reverse arr = def "reverse" [ arr ]

  let arrayStringConcat ?separator arr =
    match separator with
    | None -> def "arrayStringConcat" [ arr ]
    | Some sep -> def "arrayStringConcat" [ arr; sep ]

  (** {2 Conditional} *)
  let if_ c x y = def "if" [ c; x; y ]

  let multiIf cases ~else_ =
    let args =
      List.concat_map cases ~f:(fun (cond, result) -> [ cond; result ])
      @ [ else_ ]
    in
    def "multiIf" args

  (** {2 Comparisons} *)
  let equals x y = def "=" [ x; y ]

  let ( = ) = equals
  let notEquals x y = def "!=" [ x; y ]
  let ( != ) = notEquals
  let ( <> ) = notEquals
  let greater x y = def ">" [ x; y ]
  let ( > ) = greater
  let less x y = def "<" [ x; y ]
  let ( < ) = less
  let greaterOrEquals x y = def ">=" [ x; y ]
  let ( >= ) = greaterOrEquals
  let lessOrEquals x y = def "<=" [ x; y ]
  let ( <= ) = lessOrEquals

  (** {2 Dates and times} *)

  let toDate x = def "toDate" [ x ]
  let toDateTime x = def "toDateTime" [ x ]
  let now () = def "now" []
  let now64 () = def "now64" []
  let today () = def "today" []
  let yesterday () = def "yesterday" []
  let addDate date interval = def "addDate" [ date; interval ]
  let addDays date num = def "addDays" [ date; num ]
  let addHours datetime num = def "addHours" [ datetime; num ]
  let addInterval datetime interval = def "addInterval" [ datetime; interval ]
  let addMinutes datetime num = def "addMinutes" [ datetime; num ]
  let addMonths date num = def "addMonths" [ date; num ]
  let addSeconds datetime num = def "addSeconds" [ datetime; num ]
  let addWeeks date num = def "addWeeks" [ date; num ]
  let addYears date num = def "addYears" [ date; num ]
  let subDate date interval = def "subDate" [ date; interval ]

  let subtractInterval datetime interval =
    def "subtractInterval" [ datetime; interval ]

  let subtractDays date num = def "subtractDays" [ date; num ]
  let subtractHours datetime num = def "subtractHours" [ datetime; num ]
  let subtractMinutes datetime num = def "subtractMinutes" [ datetime; num ]
  let subtractMonths date num = def "subtractMonths" [ date; num ]
  let subtractSeconds datetime num = def "subtractSeconds" [ datetime; num ]
  let subtractWeeks date num = def "subtractWeeks" [ date; num ]
  let subtractYears date num = def "subtractYears" [ date; num ]
  let toYYYYMM date = def "toYYYYMM" [ date ]
  let toYYYYMMDD date = def "toYYYYMMDD" [ date ]
  let toStartOfYear date = def "toStartOfYear" [ date ]
  let toStartOfMonth date = def "toStartOfMonth" [ date ]
  let toStartOfWeek date = def "toStartOfWeek" [ date ]
  let toStartOfDay date = def "toStartOfDay" [ date ]
  let toStartOfHour datetime = def "toStartOfHour" [ datetime ]
  let toStartOfMinute datetime = def "toStartOfMinute" [ datetime ]
  let fromUnixTimestamp x = def "fromUnixTimestamp" [ x ]
  let toIntervalMinute x = def "toIntervalMinute" [ x ]
  let toIntervalHour x = def "toIntervalHour" [ x ]
  let toIntervalDay x = def "toIntervalDay" [ x ]
  let toIntervalWeek x = def "toIntervalWeek" [ x ]
  let toIntervalMonth x = def "toIntervalMonth" [ x ]
  let toIntervalYear x = def "toIntervalYear" [ x ]
  let toStartOfInterval x interval = def "toStartOfInterval" [ x; interval ]

  (** {2 Logical} *)

  let ( && ) x y = def "AND" [ x; y ]
  let ( &&? ) x y = match y with None -> x | Some y -> x && y
  let ( || ) x y = def "OR" [ x; y ]
  let ( ||? ) x y = match y with None -> x | Some y -> x || y
  let not_ x = def "NOT" [ x ]

  (** {2 Array} *)

  let array xs =
    Syntax.make_expr (E_call (Syntax.Func (Syntax.make_id "array"), xs))

  (** {2 Map} *)

  let map xs =
    Syntax.make_expr
      (E_call
         ( Syntax.Func (Syntax.make_id "map"),
           List.concat_map xs ~f:(fun (k, v) -> [ k; v ]) ))

  let map_get m k = def "map_get" [ m; k ]

  (** {2 Nullable} *)

  let assumeNotNull x = def "assumeNotNull" [ x ]
  let toNullable x = def "toNullable" [ x ]
  let coalesce xs ~else_ = def "coalesce" (xs @ [ else_ ])
  let nullIf x y = def "nullIf" [ x; y ]
  let ifNull x alt = def "ifNull" [ x; alt ]

  (** {2 String} *)

  let empty str = def "empty" [ str ]
  let notEmpty str = def "notEmpty" [ str ]
  let lowerUTF8 str = def "lowerUTF8" [ str ]
  let concat strs = def "concat" strs
  let substring str offset length = def "substring" [ str; offset; length ]

  let substringUTF8 str offset length =
    def "substringUTF8" [ str; offset; length ]

  let match_ hay pattern = def "match" [ hay; pattern ]

  (** {2 String replacement} *)

  let replaceOne hay needle replacement =
    def "replaceOne" [ hay; needle; replacement ]

  (** {2 String search} *)

  let like hay needle = def "like" [ hay; needle ]
  let endsWith hay needle = def "endsWith" [ hay; needle ]
  let startsWith hay needle = def "startsWith" [ hay; needle ]
  let position hay needle = def "position" [ hay; needle ]

  let positionCaseInsensitive hay needle =
    def "positionCaseInsensitive" [ hay; needle ]

  let positionUTF8 hay needle = def "positionUTF8" [ hay; needle ]

  let positionCaseInsensitiveUTF8 hay needle =
    def "positionCaseInsensitiveUTF8" [ hay; needle ]

  let locate hay needle = def "locate" [ hay; needle ]

  let multiSearchFirstPosition hay needle =
    def "multiSearchFirstPosition" [ hay; needle ]

  let multiMatchAllIndices hay patterns =
    def "multiMatchAllIndices" [ hay; patterns ]

  let extract hay pattern = def "extract" [ hay; pattern ]

  (** {2 URL functions} *)

  let extractURLParameter url name = def "extractURLParameter" [ url; name ]
  let decodeURLComponent url = def "decodeURLComponent" [ url ]

  (** {2 JSON functions} *)

  let simpleJSONExtractString json field =
    def "simpleJSONExtractString" [ json; field ]

  let simpleJSONExtractInt json field =
    def "simpleJSONExtractInt" [ json; field ]

  let simpleJSONExtractFloat json field =
    def "simpleJSONExtractFloat" [ json; field ]

  let simpleJSONExtractBool json field =
    def "simpleJSONExtractBool" [ json; field ]

  let simpleJSONExtractRaw json field =
    def "simpleJSONExtractRaw" [ json; field ]

  let jsonExtractKeys json = def "JSONExtractKeys" [ json ]

  (** {2 Splitting functions} *)

  let splitByChar sep str = def "splitByChar" [ sep; str ]

  (** {2 Type conversions} *)

  let toInt32 x = def "toInt32" [ x ]
  let toInt64 x = def "toInt64" [ x ]
  let toUInt64 x = def "toUInt64" [ x ]
  let toUInt32 x = def "toUInt32" [ x ]
  let toUInt32OrDefault x default = def "toUInt32OrDefault" [ x; default ]
  let toFloat32 x = def "toFloat32" [ x ]
  let toFloat64 x = def "toFloat64" [ x ]
  let toString x = def "toString" [ x ]
  let isFinite x = def "isFinite" [ x ]
  let isInfinite x = def "isInfinite" [ x ]
  let isNaN x = def "isNaN" [ x ]
  let ifNotFinite x y = def "ifNotFinite" [ x; y ]
  let isNull x = def "isNull" [ x ]
  let isNotNull x = def "isNotNull" [ x ]

  (** {2 Conditional} *)

  let greatest xs = def "greatest" xs
  let least xs = def "least" xs
  let avg2 x y = def "avg2" [ x; y ]

  (** {2 Hash functions} *)

  let farmFingerprint64 x = def "farmFingerprint64" [ x ]

  (** {2 Rounding functions} *)

  let round x = def "round" [ x ]

  (** {2 Bit functions} *)

  let bitAnd x y = def "bitAnd" [ x; y ]
  let bitCount x = def "bitCount" [ x ]
  let bitOr x y = def "bitOr" [ x; y ]
  let bitHammingDistance x y = def "bitHammingDistance" [ x; y ]
  let bitNot x = def "bitNot" [ x ]
  let byteSwap x = def "byteSwap" [ x ]

  (** {2 Machine learning functions} *)

  let arrayAUCPR ?partial_offsets scores labels =
    match partial_offsets with
    | None -> def "arrayAUCPR" [ scores; labels ]
    | Some offsets -> def "arrayAUCPR" [ scores; labels; offsets ]

  let arrayROCAUC ?scale ?partial_offsets scores labels =
    match (scale, partial_offsets) with
    | None, None -> def "arrayROCAUC" [ scores; labels ]
    | Some s, None -> def "arrayROCAUC" [ scores; labels; s ]
    | Some s, Some offsets -> def "arrayROCAUC" [ scores; labels; s; offsets ]
    | None, Some _ ->
        failwith "arrayROCAUC: partial_offsets requires scale to be specified"

  let arrayNormalizedGini predicted label =
    def "arrayNormalizedGini" [ predicted; label ]

  (** {1 Aggregate functions} *)

  let make_window f ?partition_by ?order_by args =
    match (partition_by, order_by) with
    | None, None -> def f args
    | _ ->
        let partition_by =
          Option.map
            (List.map ~f:(fun (A_expr expr) -> Syntax.Dimension_expr expr))
            partition_by
        in
        let order_by =
          Option.map
            (List.map ~f:(fun (A_expr expr, dir) ->
                 Syntax.Order_by_expr (expr, dir, None)))
            order_by
        in
        let window = { Syntax.partition_by; order_by } in
        Syntax.make_expr (E_window (Syntax.make_id f, args, window))

  let avg ?partition_by ?order_by x =
    make_window "avg" ?partition_by ?order_by [ x ]

  let count ?partition_by ?order_by x =
    make_window "count" ?partition_by ?order_by [ x ]

  let sum ?partition_by ?order_by x =
    make_window "sum" ?partition_by ?order_by [ x ]

  let uniq ?partition_by ?order_by x =
    make_window "uniq" ?partition_by ?order_by [ x ]

  let uniqExact ?partition_by ?order_by x =
    make_window "uniqExact" ?partition_by ?order_by [ x ]

  let min ?partition_by ?order_by x =
    make_window "min" ?partition_by ?order_by [ x ]

  let max ?partition_by ?order_by x =
    make_window "max" ?partition_by ?order_by [ x ]

  let any x = def "any" [ x ]
  let anyLast x = def "anyLast" [ x ]
  let argMin arg val_ = def "argMin" [ arg; val_ ]
  let argMax arg val_ = def "argMax" [ arg; val_ ]
  let groupArray x = def "groupArray" [ x ]
  let groupUniqArray x = def "groupUniqArray" [ x ]

  (** {2 Aggregate functions with -If suffix} *)

  let avgIf x cond = def "avgIf" [ x; cond ]
  let countIf x cond = def "countIf" [ x; cond ]
  let sumIf x cond = def "sumIf" [ x; cond ]
  let uniqIf x cond = def "uniqIf" [ x; cond ]
  let minIf x cond = def "minIf" [ x; cond ]
  let maxIf x cond = def "maxIf" [ x; cond ]
  let anyIf x cond = def "anyIf" [ x; cond ]
  let anyLastIf x cond = def "anyLastIf" [ x; cond ]
  let argMinIf arg val_ cond = def "argMinIf" [ arg; val_; cond ]
  let argMaxIf arg val_ cond = def "argMaxIf" [ arg; val_; cond ]
  let groupArrayIf x cond = def "groupArrayIf" [ x; cond ]
  let groupUniqArrayIf x cond = def "groupUniqArrayIf" [ x; cond ]

  (** {2 Aggregate functions with -State suffix} *)

  let avgState x = def "avgState" [ x ]
  let countState x = def "countState" [ x ]
  let sumState x = def "sumState" [ x ]
  let uniqState x = def "uniqState" [ x ]
  let uniqExactState x = def "uniqExactState" [ x ]
  let minState x = def "minState" [ x ]
  let maxState x = def "maxState" [ x ]
  let anyState x = def "anyState" [ x ]
  let anyLastState x = def "anyLastState" [ x ]
  let argMinState arg val_ = def "argMinState" [ arg; val_ ]
  let argMaxState arg val_ = def "argMaxState" [ arg; val_ ]
  let groupArrayState x = def "groupArrayState" [ x ]
  let groupUniqArrayState x = def "groupUniqArrayState" [ x ]

  (** {2 Aggregate functions with -StateIf suffix} *)

  let avgStateIf x cond = def "avgStateIf" [ x; cond ]
  let countStateIf x cond = def "countStateIf" [ x; cond ]
  let sumStateIf x cond = def "sumStateIf" [ x; cond ]
  let uniqStateIf x cond = def "uniqStateIf" [ x; cond ]
  let minStateIf x cond = def "minStateIf" [ x; cond ]
  let maxStateIf x cond = def "maxStateIf" [ x; cond ]
  let anyStateIf x cond = def "anyStateIf" [ x; cond ]
  let anyLastStateIf x cond = def "anyLastStateIf" [ x; cond ]
  let argMinStateIf arg val_ cond = def "argMinStateIf" [ arg; val_; cond ]
  let argMaxStateIf arg val_ cond = def "argMaxStateIf" [ arg; val_; cond ]
  let groupArrayStateIf x cond = def "groupArrayStateIf" [ x; cond ]
  let groupUniqArrayStateIf x cond = def "groupUniqArrayStateIf" [ x; cond ]

  (** {2 Aggregate functions with -Merge suffix} *)

  let avgMerge x = def "avgMerge" [ x ]
  let countMerge x = def "countMerge" [ x ]
  let sumMerge x = def "sumMerge" [ x ]
  let uniqMerge x = def "uniqMerge" [ x ]
  let uniqExactMerge x = def "uniqExactMerge" [ x ]
  let minMerge x = def "minMerge" [ x ]
  let maxMerge x = def "maxMerge" [ x ]
  let anyMerge x = def "anyMerge" [ x ]
  let anyLastMerge x = def "anyLastMerge" [ x ]
  let argMinMerge x = def "argMinMerge" [ x ]
  let argMaxMerge x = def "argMaxMerge" [ x ]
  let groupArrayMerge x = def "groupArrayMerge" [ x ]
  let groupUniqArrayMerge x = def "groupUniqArrayMerge" [ x ]

  (** {2 Aggregate combinators} *)

  let finalizeAggregation x = def "finalizeAggregation" [ x ]
  let sumForEach x = def "sumForEach" [ x ]
  let sumMap keys values = def "sumMap" [ keys; values ]
  let sumMapState keys values = def "sumMapState" [ keys; values ]
  let sumMapMerge x = def "sumMapMerge" [ x ]
  let sumMapMergeState keys values = def "sumMapMergeState" [ keys; values ]
  let avgMergeStateIf x cond = def "avgMergeStateIf" [ x; cond ]

  (** {2 Join Tables / Dictionaries} *)

  let joinGet ({ db; table; keys = _; values = _ } : _ Dict.t) value keys =
    let keys =
      match keys.Syntax.node with
      | Syntax.E_call (Syntax.Func { node = "tuple"; _ }, args) -> args
      | _ -> [ keys ]
    in
    def "joinGet" (unsafe (Printf.sprintf "%s.%s" db table) :: value :: keys)

  let joinGetOrNull = joinGet

  let dictGet ({ db; table; keys = _; values = _ } : _ Dict.t) value key =
    def "dictGet" [ unsafe (Printf.sprintf "%s.%s" db table); value; key ]

  (** {2 Tuples} *)

  let tuple2 (x, y) = def "tuple" [ x; y ]
  let tuple3 (x, y, z) = def "tuple" [ x; y; z ]
  let tuple4 (a, b, c, d) = def "tuple" [ a; b; c; d ]
end

let in_ x xs =
  match xs with
  | In_array expr -> Syntax.make_expr (Syntax.E_in (x, In_expr expr))
  | In_query select ->
      let select = select ~alias:"q" in
      let () =
        (* need to "use" the field so it materializses *)
        let scope = scope_from_select select in
        let (_ : _ expr) = scope#query (fun scope -> scope#_1) in
        ()
      in
      let select = To_syntax.to_syntax select in
      Syntax.make_expr (Syntax.E_in (x, In_query select))

let mem_field expr select =
  List.find_map select.rev_fields ~f:(function
    | A_field (expr', alias) when Syntax.equal_expr expr expr' -> Some alias
    | _ -> None)

let add_field ?(force = false) ~expr select =
  let do_add () =
    let rec alias expr =
      match expr.Syntax.node with
      | Syntax.E_col (_, name) -> Some name.node
      | Syntax.E_id id -> Some id.node
      | Syntax.E_ascribe (expr, _) -> alias expr
      | Syntax.E_query (_, expr) -> alias expr
      | _ -> None
    in
    let alias =
      match alias expr with
      | None -> Printf.sprintf "_%d" (List.length select.rev_fields + 1)
      | Some alias ->
          let rec find_unique alias n =
            let alias' =
              if n = 0 then alias else Printf.sprintf "%s_%d" alias n
            in
            if SS.mem alias' select.fields_aliases then find_unique alias (n + 1)
            else alias'
          in
          find_unique alias 0
    in
    let field = A_field (expr, alias) in
    select.rev_fields <- field :: select.rev_fields;
    select.fields_aliases <- SS.add alias select.fields_aliases;
    alias
  in
  match mem_field expr select with
  | None -> do_add ()
  | Some _ when force -> do_add ()
  | Some alias -> alias

let with_cte ?(materialized = false) ~alias:cte_alias cte_query :
    ((alias:string -> _ from_one) -> _ select) -> _ select =
 fun query ~alias ->
  let cte_query = cte_query ~alias in
  let cte =
    From_select { select = cte_query; alias = cte_alias; cluster_name = None }
  in
  let query =
    query
      (fun ~alias () ->
        let scope' = scope_from_select cte_query in
        let scope =
          (object (self)
             method query' f =
               let witness, add = scope'#query' f in
               let add ~force =
                 let x = add ~force in
                 match x.Syntax.node with
                 | E_col (_, name) -> unsafe_col alias name.node
                 | _ -> failwith "invariant violation: expected column"
               in
               (witness, add)

             method query f = snd (self#query' f) ~force:false

             method query_many f =
               let xs = scope'#query_many f in
               List.map xs ~f:(fun (A_expr e) ->
                   let e =
                     match e.Syntax.node with
                     | E_col (_, name) -> unsafe_col alias name.node
                     | _ -> failwith "invariant violation: expected column"
                   in
                   A_expr e)
           end
            : _ scope)
        in
        From_cte_ref { cte; cte_alias; scope; alias })
      ~alias
  in
  let rec add_cte = function
    | Select select ->
        let cte = Cte_query (cte_alias, cte_query, materialized) in
        select.ctes <- cte :: select.ctes
    | Union union ->
        (* TODO: not ideal as we push same CTE twice *)
        add_cte union.x;
        add_cte union.y
  in
  add_cte query;
  query

let select ~from ?prewhere ?where ?qualify ?group_by ?having ?order_by ?limit
    ?offset ?(settings = []) ~select () ~alias : _ scope select0 =
  let from = from () in
  let inner_scope = scope_from from in
  let scope' = select inner_scope in
  let prewhere =
    Option.map (fun prewhere -> A_expr (prewhere inner_scope)) prewhere
  in
  let where = Option.map (fun where -> A_expr (where inner_scope)) where in
  let qualify =
    Option.map (fun qualify -> A_expr (qualify inner_scope)) qualify
  in
  let group_by = Option.map (fun group_by -> group_by inner_scope) group_by in
  let having = Option.map (fun having -> A_expr (having inner_scope)) having in
  let order_by = Option.map (fun order_by -> order_by inner_scope) order_by in
  let limit = Option.map (fun limit -> A_expr (limit inner_scope)) limit in
  let offset = Option.map (fun offset -> A_expr (offset inner_scope)) offset in
  let rec select =
    lazy
      {
        ctes = [];
        from = A_from from;
        scope =
          (object (self)
             method query' f =
               let expr = f scope' in
               ( expr,
                 fun ~force ->
                   let c = add_field ~force ~expr (Lazy.force select) in
                   unsafe_col alias c )

             method query f = snd (self#query' f) ~force:false

             method query_many f =
               let xs = f scope' in
               List.map xs ~f:(fun (A_expr expr) ->
                   let c = add_field ~expr (Lazy.force select) in
                   A_expr (unsafe_col alias c))
           end
            : _ scope);
        prewhere;
        where;
        qualify;
        group_by;
        having;
        order_by;
        limit;
        offset;
        settings;
        rev_fields = [];
        fields_aliases = SS.empty;
      }
  in
  Select (Lazy.force select)

let union x y ~alias =
  let x = x ~alias in
  let y = y ~alias in
  let scope_x = scope_from_select x in
  let scope_y = scope_from_select y in
  let rec union =
    lazy
      {
        x;
        y;
        scope =
          object (self)
            method query' f =
              let e_x, commit_x = scope_x#query' f in
              let e_y, commit_y = scope_y#query' f in
              ( Syntax.make_expr
                  (E_call (Func (Syntax.make_id "tuple"), [ e_x; e_y ])),
                fun ~force ->
                  let union = Lazy.force union in
                  let mem_expr expr =
                    List.exists union.exprs ~f:(fun (A_expr e) ->
                        Syntax.equal_expr e expr)
                  in
                  match (mem_expr e_x, mem_expr e_y) with
                  | false, false ->
                      union.exprs <- A_expr e_x :: A_expr e_y :: union.exprs;
                      commit_y ~force:true |> ignore;
                      commit_x ~force:true
                  | true, false ->
                      union.exprs <- A_expr e_y :: union.exprs;
                      commit_y ~force:true |> ignore;
                      commit_x ~force:true
                  | false, true ->
                      union.exprs <- A_expr e_x :: union.exprs;
                      commit_y ~force:true |> ignore;
                      commit_x ~force:true
                  | true, true -> commit_x ~force )

            method query f = snd (self#query' f) ~force:false

            method query_many f =
              let xs = scope_x#query_many f in
              let _ys : a_expr list = scope_y#query_many f in
              xs
          end;
        exprs = [];
      }
  in
  Union (Lazy.force union)

let grouping_sets (gs : a_expr list list) =
  let u = unsafe in
  let cat ?sep xs =
    let xs =
      match sep with None -> xs | Some sep -> List.intersperse ~x:(u sep) xs
    in
    Syntax.make_expr (Syntax.E_unsafe_concat xs)
  in
  let gs =
    List.map gs ~f:(fun es ->
        let es = List.map es ~f:(fun (A_expr e) -> e) |> cat ~sep:", " in
        cat [ u "("; es; u ")" ])
    |> cat ~sep:", "
  in
  cat [ u "GROUPING SETS ("; gs; u ")" ]

type json =
  [ `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of json list
  | `Null
  | `String of string ]
(** JSON value (compatible with Yojson.Basic.t) *)

module Parse = struct
  exception Parse_error of json option * string

  let parse_error ?json msg = raise (Parse_error (json, msg))

  let string_of_json = function
    | `String s -> s
    | json -> parse_error ~json "expected a string"

  let int_of_json = function
    | `Int i -> i
    | `String s as json -> (
        match Int.of_string s with
        | Some i -> i
        | None -> parse_error ~json "cannot parse as integer")
    | json -> parse_error ~json "expected an integer"

  let int64_of_json = function
    | `Int i -> Int64.of_int i
    | `String s as json -> (
        match Int64.of_string_opt s with
        | Some i -> i
        | None -> parse_error ~json "cannot parse as Int64")
    | json -> parse_error ~json "cannot parse as Int64"

  let uint64_of_json = function
    | `Int i -> Unsigned.UInt64.of_int i
    | `String s as json -> (
        match Unsigned.UInt64.of_string s with
        | i -> i
        | exception Failure _ -> parse_error ~json "cannot parse as UInt64")
    | json -> parse_error ~json "cannot parse as UInt64"

  let float_of_json = function
    | `Float f -> f
    | `Int i -> Float.of_int i
    | json -> parse_error ~json "expected a float"

  let bool_of_json = function
    | `Bool b -> b
    | `Int 0 -> false
    | `Int 1 -> true
    | json -> parse_error ~json "expected a boolean"

  let strptime fmt x = ExtUnix.Specific.(timegm (strptime fmt x))

  let date_of_json = function
    | `String "0000-00-00" -> 0.
    | `String t -> strptime "%Y-%m-%d" t
    | json -> parse_error ~json "expected a Date"

  let datetime_of_json = function
    | `String "0000-00-00 00:00:00" -> 0.
    | `String t -> strptime "%Y-%m-%d %H:%M:%S" t
    | json -> parse_error ~json "expected a DateTime"

  type (_, _, _) t =
    | ANY : (_, _, json) t
    | CUSTOM :
        (json -> 'ocaml_type) * ('ocaml_type -> ('n, 's) expr)
        -> ('n, 's, 'ocaml_type) t
    | NULLABLE : (_, 's, 'o) t -> (null, 's, 'o option) t
    | VAL :
        (json -> 'ocaml_type) * ('ocaml_type -> (non_null, 'sql_type) expr)
        -> (non_null, 'sql_type, 'ocaml_type) t
    | NUMBER :
        (json -> 'ocaml_type)
        * ('ocaml_type -> (non_null, 'sql_type number) expr)
        -> (non_null, 'sql_type number, 'ocaml_type) t
    | ARRAY : ('n, 's, 'o) t -> (non_null, ('n, 's) array, 'o list) t
    | MAP :
        ('nk, 'sk, 'ok) t * ('nv, 'sv, 'ov) t
        -> (non_null, ('nk, 'sk, 'nv, 'sv) map, ('ok * 'ov) list) t

  let rec parse : type n s o. (n, s, o) t -> json -> o =
   fun parser json ->
    match parser with
    | ANY -> json
    | CUSTOM (parse, _unparse) -> parse json
    | VAL (parse, _unparse) -> parse json
    | NUMBER (parse, _unparse) -> parse json
    | NULLABLE parser -> (
        match json with `Null -> None | json -> Some (parse parser json))
    | MAP (kparser, vparser) -> (
        let parse_item = function
          | `Assoc [ ("keys", key); ("values", value) ]
          | `Assoc [ ("values", value); ("keys", key) ] ->
              (parse kparser key, parse vparser value)
          | json ->
              parse_error ~json
                {|expected an {"key":...,"value":...}, make sure output_format_json_map_as_array_of_tuples=1 is set|}
        in
        match json with
        | `List items -> List.map items ~f:parse_item
        | json ->
            parse_error ~json
              "expected an Map(K,V), make sure \
               output_format_json_map_as_array_of_tuples=1 is set")
    | ARRAY parser -> (
        match json with
        | `List jsons -> List.map jsons ~f:(parse parser)
        | json -> parse_error ~json "expected an Array(T)")

  let rec unparse : type n s o. (n, s, o) t -> o -> (n, s) expr =
   fun parser v ->
    match parser with
    | ANY -> failwith "cannot unparse ANY"
    | CUSTOM (_parse, unparse) -> unparse v
    | VAL (_parse, unparse) -> unparse v
    | NUMBER (_parse, unparse) -> unparse v
    | NULLABLE parser -> (
        match v with
        | None -> null
        | Some v -> Expr.toNullable (unparse parser v))
    | MAP (kparser, vparser) ->
        let items =
          List.map v ~f:(fun (k, v) -> (unparse kparser k, unparse vparser v))
        in
        Expr.map items
    | ARRAY parser ->
        let items = List.map v ~f:(unparse parser) in
        Expr.array items

  let nullable p = NULLABLE p
  let string = VAL (string_of_json, string)
  let bool = VAL (bool_of_json, bool)
  let int = NUMBER (int_of_json, int)
  let int64 = NUMBER (int64_of_json, int64)
  let uint64 = NUMBER (uint64_of_json, uint64)
  let float = NUMBER (float_of_json, float)
  let date = VAL (date_of_json, date)
  let datetime = VAL (datetime_of_json, datetime)
  let array p = ARRAY p
  let map k v = MAP (k, v)
  let any = ANY
  let custom (parse, unparse) = CUSTOM (parse, unparse)
end

module Row = struct
  type _ t =
    | Row_col : ('null, 'sql_type) expr * ('null, 'sql_type, 'a) Parse.t -> 'a t
    | Row_both : 'a t * 'b t -> ('a * 'b) t
    | Row_map : ('a -> 'b) * 'a t -> 'b t
    | Row_val : 'a -> 'a t
    | Row_row : a_expr list * (json list -> 'a) -> 'a t

  let ( let+ ) x f = Row_map (f, x)
  let ( and+ ) x y = Row_both (x, y)
  let return x = Row_val x
  let col e p = Row_col (e, p)
  let row es p = Row_row (es, p)

  let ignore expr =
    let+ _ = Row_col (expr, Parse.any) in
    ()

  let parse : type a. a t -> json list -> a =
    let rec aux : type a. a t -> json list -> a * json list =
     fun rowspec row ->
      match (rowspec, row) with
      | Row_val x, row -> (x, row)
      | Row_col (_, _parser), [] -> Parse.parse_error "missing a column"
      | Row_col (_, parser), col :: row -> (Parse.parse parser col, row)
      | Row_both (x, y), row ->
          let x, row = aux x row in
          let y, row = aux y row in
          ((x, y), row)
      | Row_map (f, x), row ->
          let x, row = aux x row in
          (f x, row)
      | Row_row (exprs, parse), row ->
          let rec take exprs acc row =
            match (exprs, row) with
            | [], _ -> (List.rev acc, row)
            | _ :: exprs, col :: row -> take exprs (col :: acc) row
            | _ :: _, [] -> Parse.parse_error "missing a column"
          in
          let row', row = take exprs [] row in
          let x = parse row' in
          (x, row)
    in
    fun rowspec row ->
      match aux rowspec row with
      | x, [] -> x
      | _, json -> Parse.parse_error ~json:(`List json) "extra columns in row"

  let exprs : type a. a t -> a_expr list =
    let rec aux : type a. a t -> a_expr list -> a_expr list =
     fun rowspec acc ->
      match rowspec with
      | Row_val _ -> acc
      | Row_col (expr, _) -> A_expr expr :: acc
      | Row_both (x, y) -> aux y (aux x acc)
      | Row_map (_, x) -> aux x acc
      | Row_row (exprs, _) -> List.rev_append exprs acc
    in
    fun row -> List.rev (aux row [])
end

module IM = Map.Make (struct
  type t = Syntax.id

  let compare = Syntax.compare_id
end)

let query q f =
  let q = q ~alias:"q" in
  let scope = scope_from_select q in
  let row =
    f
      (object
         method q = scope
      end)
  in
  let row_exprs = Row.exprs row in
  let plain_columns_in_order =
    (* if all expressions are just columns, return their aliases in order required *)
    let exception Cannot_unwrap in
    try
      Some
        (List.map row_exprs ~f:(fun (A_expr expr) ->
             match expr.Syntax.node with
             | E_col (_, alias) -> alias
             | _ -> raise_notrace Cannot_unwrap))
    with Cannot_unwrap -> None
  in
  let select = To_syntax.to_syntax q in
  let select_fields =
    (* if the underyling query is just a SELECT with all fields with aliases, we can re-order them easily *)
    match select.Syntax.node with
    | Syntax.Q_select { select = Select_fields fields; _ } ->
        let exception Cannot_unwrap in
        Some
          (List.fold_left fields ~init:IM.empty
             ~f:(fun map (field : Syntax.field) ->
               let alias =
                 match field.alias with
                 | Some alias -> alias
                 | None -> raise_notrace Cannot_unwrap
               in
               IM.add alias field map))
    | _ -> None
  in
  let select =
    match (plain_columns_in_order, select_fields) with
    | Some columns, Some fields -> (
        let fields =
          List.map columns ~f:(fun alias ->
              match IM.find_opt alias fields with
              | Some field -> field
              | None ->
                  fail "invariant violation: missing field for alias %s"
                    alias.node)
        in
        match select.node with
        | Syntax.Q_select select ->
            Syntax.make_query
              (Q_select { select with select = Select_fields fields })
        | _ -> fail "invariant violation: expected select query")
    | Some _, _ | None, _ ->
        let fields =
          List.map row_exprs ~f:(fun (A_expr expr) ->
              { Syntax.expr; alias = None })
        in
        Syntax.make_query
          (Syntax.Q_select
             {
               with_fields = [];
               from =
                 Syntax.make_from
                   (F
                      (Syntax.make_from_one
                         (F_select
                            {
                              select;
                              alias = Syntax.make_id "q";
                              cluster_name = None;
                            })));
               select = Select_fields fields;
               prewhere = None;
               where = None;
               qualify = None;
               group_by = None;
               having = None;
               order_by = None;
               limit = None;
               offset = None;
               settings = [];
             })
  in
  let sql = Ch_queries_syntax.Printer.print_query select in
  let parse_row = Row.parse row in
  (sql, parse_row)
