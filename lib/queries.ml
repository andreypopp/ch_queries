module Parser = Parser
module Lexer = Lexer
module Syntax = Syntax
module Printer = Printer
module Loc = Loc

(** Expression syntax *)
module Expr : sig
  type null = [ `not_null | `null ]
  type non_null = [ `not_null ]
  type 'a nullable = 'a constraint 'a = [< null ]
  type 'a number = private A_number
  type (+'null, +'typ) t

  val assumeNotNull : ([< null ] nullable, 'b) t -> (non_null, 'a) t
  val toNullable : (_, 'a) t -> (null nullable, 'a) t
  val unsafe_id : string -> _ t
  val int : int -> (non_null, int number) t
  val string : string -> (non_null, string) t
  val bool : bool -> (non_null, bool) t
  val float : float -> (non_null, float number) t
  val null : (null, 'a) t
  val coalesce : ('b, 'a) t -> ('n, 'a) t -> ('n, 'a) t
  val eq : ('n, 'a) t -> ('n, 'a) t -> ('n, bool) t
  val ( = ) : ('n, 'a) t -> ('n, 'a) t -> ('n, bool) t
  val add : ('n, 'a number) t -> ('n, 'a number) t -> ('n, 'a number) t
  val sub : ('n, 'a number) t -> ('n, 'a number) t -> ('n, 'a number) t
  val mul : ('n, 'a number) t -> ('n, 'a number) t -> ('n, 'a number) t
  val div : ('n, 'a number) t -> ('n, 'a number) t -> ('n, 'a number) t
  val ( && ) : ([< null ], bool) t -> ([< null ], bool) t -> ([> null ], bool) t
  val ( || ) : ('n, bool) t -> ('n, bool) t -> ('n, bool) t
  val not_ : ('n, bool) t -> ('n, bool) t

  val to_syntax : _ t -> Syntax.expr
  (** Convert the expression to an untyped syntax representation. *)

  val to_sql : _ t -> string
  (** Convert the expression to a SQL string. *)
end = struct
  type null = [ `null | `not_null ]
  type non_null = [ `not_null ]
  type 'a nullable = [< null ] as 'a
  type 'a number = private A_number

  type lit =
    | L_int of int
    | L_float of float
    | L_string of string
    | L_bool of bool
    | L_null

  type (+'null, +'typ) t =
    | E_id : string -> _ t
    | E_lit : lit -> _ t
    | E_app : string * args -> _ t

  and args = [] : args | ( :: ) : _ t * args -> args

  let assumeNotNull x = E_app ("assumeNotNull", [ x ])
  let toNullable x = E_app ("toNullable", [ x ])
  let unsafe_id x = E_id x
  let int x = E_lit (L_int x)
  let string x = E_lit (L_string x)
  let bool x = E_lit (L_bool x)
  let float x = E_lit (L_float x)
  let null = E_lit L_null
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

  let rec to_syntax : type n typ. (n, typ) t -> Syntax.expr = function
    | E_id name -> Loc.with_dummy_loc (Syntax.E_value (Loc.with_dummy_loc name))
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
          | expr :: rest -> to_syntax expr :: convert_args rest
        in
        let args = convert_args args in
        Loc.with_dummy_loc (Syntax.E_call (Loc.with_dummy_loc name, args))

  let to_sql e =
    let syn = to_syntax e in
    Printer.print_expr syn
end

module Query : sig
  type 'a scope = < query : 'n 'e. ('a -> ('n, 'e) Expr.t) -> ('n, 'e) Expr.t >

  type 'a nullable_scope =
    < query : 'n 'e. ('a -> ('n, 'e) Expr.t) -> (Expr.null, 'e) Expr.t >

  type 'scope select
  type 'a from_one
  type 'a from

  val union :
    'a scope select ->
    'a scope select ->
    ('a scope -> 'a scope -> 'a scope) ->
    'a scope select

  val select :
    from:'a from ->
    ?where:('a -> ('n, bool) Expr.t) ->
    select:('a -> 'b) ->
    unit ->
    'b scope select

  val from_select : alias:string -> 'a scope select -> 'a scope from_one

  val from_table :
    db:string ->
    table:string ->
    (alias:string -> 'a) ->
    alias:string ->
    'a scope from_one

  val from : 'a from_one -> 'a from

  val join :
    'a from ->
    'b scope from_one ->
    on:('a * 'b scope -> ('c, bool) Expr.t) ->
    ('a * 'b scope) from

  val left_join :
    'a from ->
    'b scope from_one ->
    on:('a * 'b scope -> ('c, bool) Expr.t) ->
    ('a * 'b nullable_scope) from

  val to_syntax : _ select -> Syntax.query
  val to_sql : _ select -> string
  val use : 'a scope select -> ('a scope -> 'b) -> string * 'b
end = struct
  type 'a scope = < query : 'n 'e. ('a -> ('n, 'e) Expr.t) -> ('n, 'e) Expr.t >

  type 'a nullable_scope =
    < query : 'n 'e. ('a -> ('n, 'e) Expr.t) -> (Expr.null, 'e) Expr.t >

  type a_expr = A_expr : _ Expr.t -> a_expr

  type a_field = A_field : _ Expr.t * string -> a_field
  [@@ocaml.warning "-37"]

  type 'scope select0 =
    | Select of {
        from : a_from0;
        scope : 'scope;  (** scope of the query, once it is queried *)
        where : a_expr option;
        mutable fields : a_field list;
            (** list of fields build within the SELECT *)
      }
    | Union of {
        x : 'scope select0;
        y : 'scope select0;
        scope : 'scope -> 'scope -> 'scope;
      }

  and 'scope select = alias:string -> 'scope select0

  and 'a from_one0 =
    | From_table : {
        db : string;
        table : string;
        alias : string;
        scope : 'a;
      }
        -> 'a from_one0
    | From_select : { select : 'a select0; alias : string } -> 'a from_one0

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

  let scope_from : type scope. scope from0 -> scope = function
    | From { scope; _ } -> scope
    | From_join { scope; _ } -> scope

  let from_select ~alias select () =
    From_select { select = select ~alias; alias }

  let from_table ~db ~table scope ~alias () =
    let scope = scope ~alias in
    let scope : _ scope =
      object
        method query : 'n 'e. (_ -> ('n, 'e) Expr.t) -> ('n, 'e) Expr.t =
          fun f ->
            let e = f scope in
            e
      end
    in
    From_table { db; table; alias; scope }

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

  let select ~from ?where ~select () ~alias =
    let from = from () in
    let inner_scope = scope_from from in
    let scope' = select inner_scope in
    let where = Option.map (fun where -> A_expr (where inner_scope)) where in
    let rec select =
      lazy
        (Select
           {
             from = A_from from;
             scope =
               object
                 method query : 'n 'e. (_ -> ('n, 'e) Expr.t) -> ('n, 'e) Expr.t
                     =
                   fun f ->
                     let e = f scope' in
                     let c = add_field e (Lazy.force select) in
                     Expr.unsafe_id (Printf.sprintf "%s.%s" alias c)
               end;
             where;
             fields = [];
           })
    in
    Lazy.force select

  let scope_from_one = function
    | From_table { scope; _ } -> scope
    | From_select { select; alias = _ } ->
        let rec select_scope select =
          match select with
          | Select { scope; _ } -> scope
          | Union { x; y; scope } -> scope (select_scope x) (select_scope y)
        in
        select_scope select

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
        method query : 'n 'e. (_ -> ('n, 'e) Expr.t) -> ('n, 'e) Expr.t =
          fun f -> (scope_from_one join)#query f
      end
    in
    let on = on (scope_from, scope_join) in
    let scope_join : _ nullable_scope =
      object
        method query : 'n 'e. (_ -> ('n, 'e) Expr.t) -> (Expr.null, 'e) Expr.t =
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

  let union x y scope ~alias = Union { x = x ~alias; y = y ~alias; scope }

  let to_syntax' q =
    let rec select_to_syntax : type a. a select0 -> Syntax.querysyn = function
      | Select { from = A_from from; where; fields; scope = _ } ->
          let syntax_fields =
            List.rev_map
              ~f:(fun (A_field (expr, alias)) ->
                {
                  Syntax.expr = Expr.to_syntax expr;
                  alias = Some (Loc.with_dummy_loc alias);
                })
              fields
          in
          let syntax_where =
            Option.map (fun (A_expr expr) -> Expr.to_syntax expr) where
          in
          let syntax_from = from_to_syntax from in
          {
            Syntax.fields = syntax_fields;
            from = syntax_from;
            where = syntax_where;
          }
      | Union { x = _; y = _; _ } ->
          failwith "TODO: union queries not yet supported"
    and from_one_to_syntax : type a. a from_one0 -> Syntax.from_one = function
      | From_table { db; table; alias; _ } ->
          Loc.with_dummy_loc
            (Syntax.F_table
               {
                 db = Loc.with_dummy_loc db;
                 table = Loc.with_dummy_loc table;
                 alias = Loc.with_dummy_loc alias;
               })
      | From_select { select; alias } ->
          Loc.with_dummy_loc
            (Syntax.F_select
               {
                 select = Loc.with_dummy_loc (select_to_syntax select);
                 alias = Loc.with_dummy_loc alias;
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
                 on = Expr.to_syntax on;
               })
    in
    Loc.with_dummy_loc (select_to_syntax q)

  let to_sql' q =
    let syn = to_syntax' q in
    Printer.print_query syn

  let to_syntax q = to_syntax' (q ~alias:"_")
  let to_sql q = to_sql' (q ~alias:"_")

  let use q f =
    match q ~alias:"_" with
    | Select { scope; _ } as q ->
        let v = f scope in
        (to_sql' q, v)
    | Union _ -> failwith "TODO"
end

include Query
