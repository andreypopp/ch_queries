module Syntax = Ch_queries_syntax.Syntax

type null = [ `null | `not_null ]
type non_null = [ `not_null ]
type 'a nullable = [< null ] as 'a
type 'a number = private A_number
type date = private Date
type datetime = private DateTime
type ('null, 'a) array = private A_array
type ('null, 'a) typ = private A_typ
type ('x, 'y) tuple2 = private A_tuple2

type (+'null, +'typ) expr = Syntax.expr

and 'a in_rhs =
  | In_query : < _1 : (_, 'a) expr > scope select -> 'a in_rhs
  | In_array : (_, (_, 'a) array) expr -> 'a in_rhs

and a_expr = A_expr : _ expr -> a_expr

and 'a scope =
  < query : 'n 'e. ('a -> ('n, 'e) expr) -> ('n, 'e) expr
  ; query_many : ('a -> a_expr list) -> a_expr list >

and 'a nullable_scope =
  < query : 'n 'e. ('a -> ('n, 'e) expr) -> (null, 'e) expr
  ; query_many : ('a -> a_expr list) -> a_expr list >

and a_field = A_field : Ch_queries_syntax.Syntax.expr * string -> a_field

and 'scope select0 =
  | Select of {
      from : a_from0;
      scope : 'scope;  (** scope of the query, once it is queried *)
      prewhere : a_expr option;
      where : a_expr option;
      qualify : a_expr option;
      group_by : a_expr list option;
      having : a_expr option;
      order_by : (a_expr * [ `ASC | `DESC ]) list option;
      limit : a_expr option;
      offset : a_expr option;
      settings :
        (string * [ `Int of int | `String of string | `Bool of bool ]) list;
      mutable fields : a_field list;
          (** list of fields build within the SELECT *)
    }
  | Union of { x : 'scope select0; y : 'scope select0 }

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

and 'a from = unit -> 'a from0
and a_from0 = A_from : 'a from0 -> a_from0
and a_from_one0 = A_from_one : 'a from_one0 -> a_from_one0

let scope_from : type scope. scope from0 -> scope = function
  | From { scope; _ } -> scope
  | From_join { scope; _ } -> scope

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
             object
               method query : 'n 'e. (_ -> ('n, 'e) expr) -> ('n, 'e) expr =
                 fun f ->
                   let () =
                     match Lazy.force t with
                     | From_table t -> t.used <- true
                     | _ -> ()
                   in
                   f scope

               method query_many =
                 fun f ->
                   let () =
                     match Lazy.force t with
                     | From_table t -> t.used <- true
                     | _ -> ()
                   in
                   f scope
             end;
           final;
           used = false;
         })
  in
  Lazy.force t

let rec scope_from_select select : _ scope =
  match select with
  | Select { scope; _ } -> scope
  | Union { x; y } ->
      let scope_x = scope_from_select x in
      let scope_y = scope_from_select y in
      object
        method query : 'n 'e. (_ -> ('n, 'e) expr) -> ('n, 'e) expr =
          fun f ->
            let e_x = scope_x#query f in
            let _e_y : _ expr = scope_y#query f in
            e_x

        method query_many : (_ -> a_expr list) -> a_expr list =
          fun f ->
            let xs = scope_x#query_many f in
            let _ys : a_expr list = scope_y#query_many f in
            xs
      end

let scope_from_one = function
  | From_table { scope; _ } -> scope
  | From_select { select; alias = _; cluster_name = _ } ->
      scope_from_select select

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
      method query : 'n 'e. (_ -> ('n, 'e) expr) -> ('n, 'e) expr =
        fun f -> (scope_from_one join)#query f

      method query_many = fun f -> (scope_from_one join)#query_many f
    end
  in
  let scope_join : _ nullable_scope =
    object
      method query : 'n 'e. (_ -> ('n, 'e) expr) -> (null, 'e) expr =
        fun f -> scope_join'#query f

      method query_many = fun f -> scope_join'#query_many f
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

module To_syntax = struct
  open Ch_queries_syntax

  let rec group_by_to_syntax dimensions =
    List.map dimensions ~f:(fun (A_expr expr) -> Syntax.Dimension_expr expr)

  and order_by_to_syntax order_by =
    List.map order_by ~f:(fun (A_expr expr, dir) ->
        Syntax.Order_by_expr (expr, dir))

  and to_syntax : type a. a select0 -> Syntax.query =
   fun q ->
    let rec select_to_syntax : type a. a select0 -> Syntax.querysyn = function
      | Select
          {
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
            fields;
            scope = _;
          } ->
          let select =
            List.rev_map
              ~f:(fun (A_field (expr, alias)) ->
                { Syntax.expr; alias = Some (Syntax.make_id alias) })
              fields
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
          Q_select
            {
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
      | Union { x; y } -> Q_union (to_syntax x, to_syntax y)
    and from_one_to_syntax : type a. a from_one0 -> Syntax.from_one = function
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
    and from_to_syntax : type a. a from0 -> Syntax.from = function
      | From { from = A_from_one from; _ } ->
          Syntax.make_from (F (from_one_to_syntax from))
      | From_join { kind; from = A_from from; join = A_from_one join; on; _ }
        -> (
          let translate () =
            let kind =
              match kind with
              | `INNER_JOIN -> `INNER_JOIN
              | `LEFT_JOIN | `LEFT_JOIN_OPTIONAL -> `LEFT_JOIN
            in
            let (A_expr on) = Lazy.force on in
            Syntax.make_from
              (F_join
                 {
                   kind;
                   from = from_to_syntax from;
                   join = from_one_to_syntax join;
                   on;
                 })
          in
          match kind with
          | `LEFT_JOIN_OPTIONAL ->
              let should_eliminate =
                match join with
                | From_table { used; _ } -> not used
                | From_select { select; _ } ->
                    let rec fields = function
                      | Select { fields; _ } -> fields
                      | Union { x; y = _ } -> fields x
                    in
                    List.is_empty (fields select)
              in
              if not should_eliminate then translate () else from_to_syntax from
          | `LEFT_JOIN | `INNER_JOIN -> translate ())
    in
    Syntax.make_query (select_to_syntax q)
end

let unsafe x = Syntax.make_expr (E_unsafe (Syntax.make_id x))

let unsafe_concat xs =
  let xs = List.map xs ~f:(fun (A_expr expr) -> expr) in
  Syntax.make_expr (Syntax.E_unsafe_concat xs)

let lit lit = Syntax.make_expr (Syntax.E_lit lit)
let int x = lit (L_int x)
let string x = lit (L_string x)
let bool x = lit (L_bool x)
let float x = lit (L_float x)
let null = lit L_null

let lambda :
    string ->
    (('pn, 'pa) expr -> ('n, 'a) expr) ->
    (non_null, ('pn, 'pa) expr -> ('n, 'a) expr) expr =
 fun param f ->
  let body = f (unsafe param) in
  Syntax.make_expr (E_lambda (Syntax.make_id param, body))

let array xs = Syntax.make_expr (E_call (Syntax.Func (Syntax.make_id "["), xs))

module Expr = struct
  let def n args =
    Syntax.make_expr (Syntax.E_call (Syntax.Func (Syntax.make_id n), args))

  let assumeNotNull x = def "assumeNotNull" [ x ]
  let toNullable x = def "toNullable" [ x ]
  let coalesce x y = def "coalesce" [ x; y ]
  let plus x y = def "+" [ x; y ]
  let ( + ) = plus
  let minus x y = def "-" [ x; y ]
  let ( - ) = minus
  let multiply x y = def "*" [ x; y ]
  let ( * ) = multiply
  let divide x y = def "/" [ x; y ]
  let ( / ) = divide
  let eq x y = def "=" [ x; y ]
  let ( = ) = eq
  let gt x y = def ">" [ x; y ]
  let ( > ) = gt
  let lt x y = def "<" [ x; y ]
  let ( < ) = lt
  let ge x y = def ">=" [ x; y ]
  let ( >= ) = ge
  let le x y = def "<=" [ x; y ]
  let ( <= ) = le
  let ( && ) x y = def "and" [ x; y ]
  let ( || ) x y = def "or" [ x; y ]
  let not_ x = def "not" [ x ]
  let arrayFilter f x = def "arrayFilter" [ f; x ]
  let length x = def "length" [ x ]
  let toDate x = def "toDate" [ x ]
  let if_ c x y = def "if" [ c; x; y ]

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
                 Syntax.Order_by_expr (expr, dir)))
            order_by
        in
        let window = { Syntax.partition_by; order_by } in
        Syntax.make_expr (E_window (Syntax.make_id f, args, window))

  let count ?partition_by ?order_by x =
    make_window ?partition_by ?order_by "count" [ x ]

  let sum ?partition_by ?order_by x =
    make_window ?partition_by ?order_by "sum" [ x ]

  let uniq ?partition_by ?order_by x =
    make_window ?partition_by ?order_by "uniq" [ x ]
end

let in_ x xs =
  match xs with
  | In_array expr -> Syntax.make_expr (Syntax.E_in (x, In_expr expr))
  | In_query select ->
      let select = select ~alias:"q" in
      let () =
        (* need to "use" the field so it materializses *)
        let scope = scope_from_select select in
        let _ : _ expr = scope#query (fun scope -> scope#_1) in
        ()
      in
      let select = To_syntax.to_syntax select in
      Syntax.make_expr (Syntax.E_in (x, In_query select))

let rec add_field expr select =
  match select with
  | Select s -> (
      match
        List.find_map s.fields ~f:(function
          | A_field (expr', alias) when Syntax.equal_expr expr expr' ->
              Some alias
          | _ -> None)
      with
      | None ->
          let alias = Printf.sprintf "_%d" (List.length s.fields + 1) in
          let field = A_field (expr, alias) in
          s.fields <- field :: s.fields;
          alias
      | Some alias -> alias)
  | Union u ->
      let alias = add_field expr u.x in
      let _alias = add_field expr u.y in
      alias

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
      (Select
         {
           from = A_from from;
           scope =
             object
               method query : 'n 'e. (_ -> ('n, 'e) expr) -> ('n, 'e) expr =
                 fun f ->
                   let e = f scope' in
                   let c = add_field e (Lazy.force select) in
                   unsafe (Printf.sprintf "%s.%s" alias c)

               method query_many =
                 fun f ->
                   let xs = f scope' in
                   List.map xs ~f:(fun (A_expr e) ->
                       let c = add_field e (Lazy.force select) in
                       A_expr (unsafe (Printf.sprintf "%s.%s" alias c)))
             end;
           prewhere;
           where;
           qualify;
           group_by;
           having;
           order_by;
           limit;
           offset;
           settings;
           fields = [];
         })
  in
  Lazy.force select

let union x y ~alias = Union { x = x ~alias; y = y ~alias }

let grouping_sets (exprs : a_expr list list) =
  let concat xs = Syntax.make_expr (Syntax.E_unsafe_concat xs) in
  let exprs =
    List.map exprs ~f:(fun exprs ->
        let exprs =
          List.map exprs ~f:(fun (A_expr expr) -> expr)
          |> List.intersperse ~x:(unsafe ", ")
        in
        concat ([ unsafe "(" ] @ exprs @ [ unsafe ")" ]))
    |> List.intersperse ~x:(unsafe ", ")
  in
  concat ([ unsafe "GROUPING SETS (" ] @ exprs @ [ unsafe ")" ])

type json =
  [ `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of json list
  | `Null
  | `String of string ]
(** JSON value (compatible with Yojson.Basic.t) *)

module Row = struct
  exception Parse_error of string

  let parse_error msg = raise (Parse_error msg)

  let string_of_json = function
    | `String s -> s
    | _ -> parse_error "expected a string"

  let int_of_json = function
    | `Int i -> i
    | _ -> parse_error "expected an integer"

  let int64_of_json = function
    | `Int i -> Int64.of_int i
    | `String s -> (
        match Int64.of_string_opt s with
        | Some i -> i
        | None -> parse_error "expected an integer")
    | _ -> parse_error "expected an integer"

  let float_of_json = function
    | `Float f -> f
    | _ -> parse_error "expected a float"

  let bool_of_json = function
    | `Bool b -> b
    | _ -> parse_error "expected a boolean"

  type _ t =
    | Row_col : (non_null, 'a) expr * (json -> 'a) -> 'a t
    | Row_col_opt : (_ nullable, 'a) expr * (json -> 'a) -> 'a option t
    | Row_col_number : (non_null, 'a number) expr * (json -> 'a) -> 'a t
    | Row_col_number_opt :
        (_ nullable, 'a number) expr * (json -> 'a)
        -> 'a option t
    | Row_both : 'a t * 'b t -> ('a * 'b) t
    | Row_map : ('a -> 'b) * 'a t -> 'b t
    | Row_val : 'a -> 'a t

  let ( let+ ) x f = Row_map (f, x)
  let ( and+ ) x y = Row_both (x, y)
  let return x = Row_val x
  let string expr = Row_col (expr, string_of_json)
  let string_opt expr = Row_col_opt (expr, string_of_json)
  let bool expr = Row_col (expr, bool_of_json)
  let bool_opt expr = Row_col_opt (expr, bool_of_json)
  let int expr = Row_col_number (expr, int_of_json)
  let int_opt expr = Row_col_number_opt (expr, int_of_json)
  let int64 expr = Row_col_number (expr, int64_of_json)
  let int64_opt expr = Row_col_number_opt (expr, int64_of_json)
  let float expr = Row_col_number (expr, float_of_json)
  let float_opt expr = Row_col_number_opt (expr, float_of_json)

  let parse : type a. a t -> json list -> a =
    let rec aux : type a. a t -> json list -> a * json list =
     fun rowspec row ->
      match (rowspec, row) with
      | Row_val x, row -> (x, row)
      | Row_col (_, _parse), [] -> parse_error "missing a column"
      | Row_col (_, parse), col :: row -> (parse col, row)
      | Row_col_opt (_, _parse), `Null :: row -> (None, row)
      | Row_col_opt (_, _parse), [] -> parse_error "missing a column"
      | Row_col_opt (_, parse), col :: row -> (Some (parse col), row)
      | Row_col_number (_, _parse), [] -> parse_error "missing a column"
      | Row_col_number (_, parse), col :: row -> (parse col, row)
      | Row_col_number_opt (_, _parse), `Null :: row -> (None, row)
      | Row_col_number_opt (_, _parse), [] -> parse_error "missing a column"
      | Row_col_number_opt (_, parse), col :: row -> (Some (parse col), row)
      | Row_both (x, y), row ->
          let x, row = aux x row in
          let y, row = aux y row in
          ((x, y), row)
      | Row_map (f, x), row ->
          let x, row = aux x row in
          (f x, row)
    in
    fun rowspec row ->
      match aux rowspec row with
      | x, [] -> x
      | _, _ -> parse_error "extra columns in row"

  let fields : type a. a t -> a_expr list =
    let rec aux : type a. a t -> a_expr list -> a_expr list =
     fun rowspec acc ->
      match rowspec with
      | Row_val _ -> acc
      | Row_col (expr, _) -> A_expr expr :: acc
      | Row_col_opt (expr, _) -> A_expr expr :: acc
      | Row_col_number (expr, _) -> A_expr expr :: acc
      | Row_col_number_opt (expr, _) -> A_expr expr :: acc
      | Row_both (x, y) -> aux y (aux x acc)
      | Row_map (_, x) -> aux x acc
    in
    fun row -> List.rev (aux row [])
end

let query q f =
  let q = q ~alias:"q" in
  let scope = scope_from_select q in
  let row = f scope in
  let fields = Row.fields row in
  let fields =
    List.map fields ~f:(fun (A_expr expr) -> { Syntax.expr; alias = None })
  in
  let select = To_syntax.to_syntax q in
  let select =
    Syntax.Q_select
      {
        from =
          Syntax.make_from
            (F
               (Syntax.make_from_one
                  (F_select
                     { select; alias = Syntax.make_id "q"; cluster_name = None })));
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
      }
  in
  let sql = Ch_queries_syntax.Printer.print_query (Syntax.make_query select) in
  let parse_row = Row.parse row in
  (sql, parse_row)
