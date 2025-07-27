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
    alias:string ->
    'b scope from_one

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
end = struct
  type 'a scope = < query : 'n 'e. ('a -> ('n, 'e) Expr.t) -> ('n, 'e) Expr.t >

  type 'a nullable_scope =
    < query : 'n 'e. ('a -> ('n, 'e) Expr.t) -> (Expr.null, 'e) Expr.t >

  type a_expr = A_expr : _ Expr.t -> a_expr

  type a_field = A_field : _ Expr.t * string -> a_field
  [@@ocaml.warning "-37"]

  type 'scope select =
    | Select of {
        from : a_from;
        scope : string -> 'scope;  (** scope of the query, once it is queried *)
        where : a_expr option;
        mutable fields : a_field list;
            (** list of fields build within the SELECT *)
      }
    | Union of {
        x : 'scope select;
        y : 'scope select;
        scope : 'scope -> 'scope -> 'scope;
      }

  and 'a from_one =
    | From_table : {
        db : string;
        table : string;
        alias : string;
        scope : 'a;
      }
        -> 'a from_one
    | From_select : { select : 'a select; alias : string } -> 'a from_one

  and 'a from =
    | From : { scope : 'a; from : a_from_one } -> 'a from
    | From_join : {
        scope : 'a * 'b;
        kind : [ `INNER_JOIN | `LEFT_JOIN ];
        from : a_from;
        join : a_from_one;
        on : a_expr;
      }
        -> ('a * 'b) from

  and a_from = A_from : 'a from -> a_from
  and a_from_one = A_from_one : 'a from_one -> a_from_one

  let scope_from : type scope. scope from -> scope = function
    | From { scope; _ } -> scope
    | From_join { scope; _ } -> scope

  let from_select ~alias select = From_select { select; alias }

  let from_table ~db ~table scope ~alias =
    let scope = scope ~alias in
    let scope : _ scope =
      object
        method query : 'n 'e. (_ -> ('n, 'e) Expr.t) -> ('n, 'e) Expr.t =
          fun f -> f scope
      end
    in
    From_table { db; table; alias; scope }

  let select ~from ?where ~select () =
    let inner_scope = scope_from from in
    let scope = select inner_scope in
    let scope _name =
      object
        method query : 'n 'e. (_ -> ('n, 'e) Expr.t) -> ('n, 'e) Expr.t =
          fun f -> f scope
      end
    in
    let where = Option.map (fun where -> A_expr (where inner_scope)) where in
    from_select (Select { from = A_from from; scope; where; fields = [] })

  let scope_from_one = function
    | From_table { scope; _ } -> scope
    | From_select { select; alias } ->
        let rec select_scope select =
          match select with
          | Select { scope; _ } -> scope alias
          | Union { x; y; scope } -> scope (select_scope x) (select_scope y)
        in
        select_scope select

  let from from =
    let scope = scope_from_one from in
    From { scope; from = A_from_one from }

  let join from join ~on =
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

  let left_join from (join : 'a scope from_one) ~on =
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

  let union x y scope = Union { x; y; scope }
end

include Query
