type null = [ `null | `not_null ]
(** Expression syntax *)

type non_null = [ `not_null ]
type 'a nullable = [< null ] as 'a
type 'a number = private A_number
type ('null, 'a) array = private A_array

type lit =
  | L_int of int
  | L_float of float
  | L_string of string
  | L_bool of bool
  | L_null

type (+'null, +'typ) expr =
  | E_id : string -> _ expr
  | E_lit : lit -> _ expr
  | E_app : string * args -> _ expr
  | E_window : string * args * window -> _ expr
  | E_in : (_, 'a) expr * 'a in_rhs -> _ expr
  | E_lambda : string * _ expr -> _ expr
  | E_concat : args -> _ expr

and args = [] : args | ( :: ) : _ expr * args -> args

and window = {
  partition_by : a_expr list option;
  order_by : (a_expr * [ `ASC | `DESC ]) list option;
}

and 'a in_rhs =
  | In_query : < _1 : (_, 'a) expr > scope select -> 'a in_rhs
  | In_array : (_, (_, 'a) array) expr -> 'a in_rhs

and a_expr = A_expr : _ expr -> a_expr
and 'a scope = < query : 'n 'e. ('a -> ('n, 'e) expr) -> ('n, 'e) expr >

and 'a nullable_scope =
  < query : 'n 'e. ('a -> ('n, 'e) expr) -> (null, 'e) expr >

and a_field = A_field : _ expr * string -> a_field [@@ocaml.warning "-37"]

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
      kind : [ `INNER_JOIN | `LEFT_JOIN ];
      from : a_from0;
      join : a_from_one0;
      on : a_expr;
    }
      -> ('a * 'b) from0

and 'a from = unit -> 'a from0
and a_from0 = A_from : 'a from0 -> a_from0
and a_from_one0 = A_from_one : 'a from_one0 -> a_from_one0

let unsafe_expr x = E_id x
let unsafe_concat xs = E_concat xs
let int x = E_lit (L_int x)
let string x = E_lit (L_string x)
let bool x = E_lit (L_bool x)
let float x = E_lit (L_float x)
let null = E_lit L_null

let lambda :
    string ->
    (('pn, 'pa) expr -> ('n, 'a) expr) ->
    (non_null, ('pn, 'pa) expr -> ('n, 'a) expr) expr =
 fun param f ->
  let body = f (unsafe_expr param) in
  E_lambda (param, body)

let in_ x select = E_in (x, select)

let array xs =
  let rec args : _ expr list -> args = function
    | [] -> []
    | x :: xs -> x :: args xs
  in
  E_app ("[", args xs)

module Expr = struct
  let assumeNotNull x = E_app ("assumeNotNull", [ x ])
  let toNullable x = E_app ("toNullable", [ x ])
  let coalesce x y = E_app ("coalesce", [ x; y ])
  let eq x y = E_app ("=", [ x; y ])
  let ( = ) = eq
  let add x y = E_app ("+", [ x; y ])
  let sub x y = E_app ("-", [ x; y ])
  let mul x y = E_app ("*", [ x; y ])
  let div x y = E_app ("/", [ x; y ])
  let ( && ) x y = E_app ("and", [ x; y ])
  let ( || ) x y = E_app ("or", [ x; y ])
  let not_ x = E_app ("not", [ x ])
  let arrayFilter f x = E_app ("arrayFilter", [ f; x ])
  let length x = E_app ("length", [ x ])

  let make_window f ?partition_by ?order_by args =
    match (partition_by, order_by) with
    | None, None -> E_app (f, args)
    | _ ->
        let window = { partition_by; order_by } in
        E_window (f, args, window)

  let count ?partition_by ?order_by x =
    make_window ?partition_by ?order_by "count" [ x ]

  let sum ?partition_by ?order_by x =
    make_window ?partition_by ?order_by "sum" [ x ]

  let uniq ?partition_by ?order_by x =
    make_window ?partition_by ?order_by "uniq" [ x ]
end

let scope_from : type scope. scope from0 -> scope = function
  | From { scope; _ } -> scope
  | From_join { scope; _ } -> scope

let from_select ?cluster_name ~alias select () =
  From_select { select = select ~alias; alias; cluster_name }

let from_table ~db ~table scope =
 fun ~final ~alias () ->
  let scope = scope ~alias in
  let scope : _ scope =
    object
      method query : 'n 'e. (_ -> ('n, 'e) expr) -> ('n, 'e) expr =
        fun f ->
          let e = f scope in
          e
    end
  in
  From_table { db; table; alias; scope; final }

let rec add_field expr select =
  match select with
  | Select s ->
      let alias = Printf.sprintf "_%d" (List.length s.fields + 1) in
      let field = A_field (expr, alias) in
      s.fields <- field :: s.fields;
      alias
  | Union u ->
      let alias = add_field expr u.x in
      let _alias = add_field expr u.y in
      alias

let select ~from ?prewhere ?where ?qualify ?group_by ?having ?order_by ?limit
    ?offset ~select () ~alias =
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
                   unsafe_expr (Printf.sprintf "%s.%s" alias c)
             end;
           prewhere;
           where;
           qualify;
           group_by;
           having;
           order_by;
           limit;
           offset;
           fields = [];
         })
  in
  Lazy.force select

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
  let on = on scope in
  From_join
    {
      scope;
      kind = `INNER_JOIN;
      from = A_from from;
      join = A_from_one join;
      on = A_expr on;
    }

let left_join from (join : 'a scope from_one) ~on () =
  let from = from () in
  let join = join () in
  let scope_from = scope_from from in
  let scope_join : _ scope =
    object
      method query : 'n 'e. (_ -> ('n, 'e) expr) -> ('n, 'e) expr =
        fun f -> (scope_from_one join)#query f
    end
  in
  let on = on (scope_from, scope_join) in
  let scope_join : _ nullable_scope =
    object
      method query : 'n 'e. (_ -> ('n, 'e) expr) -> (null, 'e) expr =
        fun f -> Expr.toNullable (scope_join#query f)
    end
  in
  From_join
    {
      scope = (scope_from, scope_join);
      kind = `LEFT_JOIN;
      from = A_from from;
      join = A_from_one join;
      on = A_expr on;
    }

let union x y ~alias = Union { x = x ~alias; y = y ~alias }

module To_syntax = struct
  open Queries_syntax

  let rec expr_to_syntax : type n typ. (n, typ) expr -> Syntax.expr = function
    | E_concat args ->
        let rec convert_args : args -> Syntax.expr list = function
          | [] -> []
          | expr :: rest -> expr_to_syntax expr :: convert_args rest
        in
        Loc.with_dummy_loc (Syntax.E_concat (convert_args args))
    | E_id name ->
        Loc.with_dummy_loc (Syntax.E_value (Loc.with_dummy_loc name, None))
    | E_lit lit ->
        let syntax_lit =
          match lit with
          | L_int x -> Syntax.L_int x
          | L_bool x -> Syntax.L_bool x
          | L_string x -> Syntax.L_string x
          | L_float _ -> failwith "float literals not supported in syntax"
          | L_null -> failwith "null literals not supported in syntax"
        in
        Loc.with_dummy_loc (Syntax.E_lit syntax_lit)
    | E_app (name, args) ->
        let rec convert_args : args -> Syntax.expr list = function
          | [] -> []
          | expr :: rest -> expr_to_syntax expr :: convert_args rest
        in
        let args = convert_args args in
        Loc.with_dummy_loc
          (Syntax.E_call (Syntax.Func (Loc.with_dummy_loc name), args))
    | E_window (name, args, { partition_by; order_by }) ->
        let rec convert_args : args -> Syntax.expr list = function
          | [] -> []
          | expr :: rest -> expr_to_syntax expr :: convert_args rest
        in
        let args = convert_args args in
        let window =
          let partition_by = Option.map group_by_to_syntax partition_by in
          let order_by = Option.map order_by_to_syntax order_by in
          { Syntax.partition_by; order_by }
        in
        Loc.with_dummy_loc
          (Syntax.E_window (Loc.with_dummy_loc name, args, window))
    | E_in (expr, In_query select) ->
        let expr = expr_to_syntax expr in
        let select = select ~alias:"q" in
        let () =
          (* need to "use" the field so it materializses *)
          let scope = scope_from_select select in
          let _ : _ expr = scope#query (fun scope -> scope#_1) in
          ()
        in
        let select = to_syntax select in
        Loc.with_dummy_loc (Syntax.E_in (expr, Syntax.In_query select))
    | E_in (expr, In_array expr') ->
        let expr = expr_to_syntax expr in
        let expr' = expr_to_syntax expr' in
        Loc.with_dummy_loc (Syntax.E_in (expr, Syntax.In_expr expr'))
    | E_lambda (param, body) ->
        let param = Loc.with_dummy_loc param in
        let body = expr_to_syntax body in
        Loc.with_dummy_loc (Syntax.E_lambda (param, body))

  and group_by_to_syntax dimensions =
    List.map dimensions ~f:(fun (A_expr expr) ->
        Syntax.Dimension_expr (expr_to_syntax expr))

  and order_by_to_syntax order_by =
    List.map order_by ~f:(fun (A_expr expr, dir) ->
        Syntax.Order_by_expr (expr_to_syntax expr, dir))

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
            fields;
            scope = _;
          } ->
          let select =
            List.rev_map
              ~f:(fun (A_field (expr, alias)) ->
                {
                  Syntax.expr = expr_to_syntax expr;
                  alias = Some (Loc.with_dummy_loc alias);
                })
              fields
          in
          let prewhere =
            Option.map (fun (A_expr expr) -> expr_to_syntax expr) prewhere
          in
          let where =
            Option.map (fun (A_expr expr) -> expr_to_syntax expr) where
          in
          let qualify =
            Option.map (fun (A_expr expr) -> expr_to_syntax expr) qualify
          in
          let group_by = Option.map group_by_to_syntax group_by in
          let having =
            Option.map (fun (A_expr expr) -> expr_to_syntax expr) having
          in
          let order_by = Option.map order_by_to_syntax order_by in
          let limit =
            Option.map (fun (A_expr expr) -> expr_to_syntax expr) limit
          in
          let offset =
            Option.map (fun (A_expr expr) -> expr_to_syntax expr) offset
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
            }
      | Union { x; y } -> Q_union (to_syntax x, to_syntax y)
    and from_one_to_syntax : type a. a from_one0 -> Syntax.from_one = function
      | From_table { db; table; alias; final; _ } ->
          Loc.with_dummy_loc
            (Syntax.F_table
               {
                 db = Loc.with_dummy_loc db;
                 table = Loc.with_dummy_loc table;
                 alias = Loc.with_dummy_loc alias;
                 final;
               })
      | From_select { select; alias; cluster_name } ->
          Loc.with_dummy_loc
            (Syntax.F_select
               {
                 select = Loc.with_dummy_loc (select_to_syntax select);
                 alias = Loc.with_dummy_loc alias;
                 cluster_name =
                   Option.map
                     (fun name ->
                       Queries_syntax.Syntax.Cluster_name
                         (Loc.with_dummy_loc name))
                     cluster_name;
               })
    and from_to_syntax : type a. a from0 -> Syntax.from = function
      | From { from = A_from_one from; _ } ->
          Loc.with_dummy_loc (Syntax.F (from_one_to_syntax from))
      | From_join
          {
            kind;
            from = A_from from;
            join = A_from_one join;
            on = A_expr on;
            _;
          } ->
          Loc.with_dummy_loc
            (Syntax.F_join
               {
                 kind;
                 from = from_to_syntax from;
                 join = from_one_to_syntax join;
                 on = expr_to_syntax on;
               })
    in
    Loc.with_dummy_loc (select_to_syntax q)
end

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

  let ( let+ ) x f = Row_map (f, x)
  let ( and+ ) x y = Row_both (x, y)
  let string expr = Row_col (expr, string_of_json)
  let string_opt expr = Row_col_opt (expr, string_of_json)
  let bool expr = Row_col (expr, bool_of_json)
  let bool_opt expr = Row_col_opt (expr, bool_of_json)
  let int expr = Row_col_number (expr, int_of_json)
  let int_opt expr = Row_col_number_opt (expr, int_of_json)
  let float expr = Row_col_number (expr, float_of_json)
  let float_opt expr = Row_col_number_opt (expr, float_of_json)

  let parse : type a. a t -> json list -> a =
    let rec aux : type a. a t -> json list -> a * json list =
     fun rowspec row ->
      match (rowspec, row) with
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
    List.map fields ~f:(fun (A_expr e) ->
        {
          Queries_syntax.Syntax.expr = To_syntax.expr_to_syntax e;
          alias = None;
        })
  in
  let select = To_syntax.to_syntax q in
  let select =
    Queries_syntax.Syntax.Q_select
      {
        from =
          Queries_syntax.Loc.with_dummy_loc
            (Queries_syntax.Syntax.F
               (Queries_syntax.Loc.with_dummy_loc
                  (Queries_syntax.Syntax.F_select
                     {
                       select;
                       alias = Queries_syntax.Loc.with_dummy_loc "q";
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
      }
  in
  let sql =
    Queries_syntax.Printer.print_query
      (Queries_syntax.Loc.with_dummy_loc select)
  in
  let parse_row = Row.parse row in
  (sql, parse_row)

module Args = struct
  type t = args = [] : t | ( :: ) : _ expr * t -> t
end
