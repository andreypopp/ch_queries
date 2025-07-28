type null = [ `not_null | `null ]
type non_null = [ `not_null ]
type 'a nullable = 'a constraint 'a = [< null ]

type (+'null, +'typ) expr
(** SQL expression DSL. *)

type 'a number = private A_number

val int : int -> (non_null, int number) expr
val string : string -> (non_null, string) expr
val bool : bool -> (non_null, bool) expr
val float : float -> (non_null, float number) expr
val null : (null, 'a) expr
val unsafe_expr : string -> _ expr

val expr_to_syntax : _ expr -> Syntax.expr
(** Convert the expression to an untyped syntax representation. *)

val expr_to_sql : _ expr -> string
(** Convert the expression to a SQL string. *)

type 'a scope = < query : 'n 'e. ('a -> ('n, 'e) expr) -> ('n, 'e) expr >
(** Represents a table's or a subquery's scope. *)

type 'a nullable_scope =
  < query : 'n 'e. ('a -> ('n, 'e) expr) -> (null, 'e) expr >
(** Represents a table's or a subquery's scope when one is used on the right
    side of a LEFT JOIN. *)

type 'scope select
(** SELECT query. *)

type 'a from_one
(** Represents a single table or subquery in the FROM clause. *)

type 'a from
(** Represents the FROM clause of a query, which can contain multiple tables or
    subqueries. *)

type a_expr = A_expr : _ expr -> a_expr

val union :
  'a scope select ->
  'a scope select ->
  ('a scope -> 'a scope -> 'a scope) ->
  'a scope select

val select :
  from:'from from ->
  ?where:('from -> (_, bool) expr) ->
  ?qualify:('from -> (_, bool) expr) ->
  ?group_by:('from -> a_expr list) ->
  ?having:('from -> (_, bool) expr) ->
  ?order_by:('from -> (a_expr * [ `ASC | `DESC ]) list) ->
  ?limit:('from -> (_, int number) expr) ->
  ?offset:('from -> (_, int number) expr) ->
  select:('from -> 'select) ->
  unit ->
  'select scope select

val from : 'a from_one -> 'a from

val join :
  'a from ->
  'b scope from_one ->
  on:('a * 'b scope -> ('c, bool) expr) ->
  ('a * 'b scope) from

val left_join :
  'a from ->
  'b scope from_one ->
  on:('a * 'b scope -> ('c, bool) expr) ->
  ('a * 'b nullable_scope) from

val from_select : alias:string -> 'a scope select -> 'a scope from_one

val from_table :
  db:string ->
  table:string ->
  (alias:string -> 'a) ->
  alias:string ->
  'a scope from_one

val to_syntax : _ select -> Syntax.query
val to_sql : _ select -> string

val use : 'a scope select -> ('a scope -> 'b) -> string * 'b
(** Use the SELECT query, selecting needed values form it, it generates the SQL
    and returns the data. *)

module Expr : sig
  val assumeNotNull : ([< null ] nullable, 'b) expr -> (non_null, 'a) expr
  val toNullable : (_, 'a) expr -> (null nullable, 'a) expr
  val coalesce : ('b, 'a) expr -> ('n, 'a) expr -> ('n, 'a) expr
  val eq : ('n, 'a) expr -> ('n, 'a) expr -> ('n, bool) expr
  val ( = ) : ('n, 'a) expr -> ('n, 'a) expr -> ('n, bool) expr
  val add : ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr
  val sub : ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr
  val mul : ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr
  val div : ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr
  val ( && ) : ('n, bool) expr -> ('n, bool) expr -> ('n, bool) expr
  val ( || ) : ('n, bool) expr -> ('n, bool) expr -> ('n, bool) expr
  val not_ : ('n, bool) expr -> ('n, bool) expr

  val count :
    ?partition_by:a_expr list ->
    ?order_by:(a_expr * [ `ASC | `DESC ]) list ->
    ('n, _) expr ->
    (non_null, int number) expr

  val sum :
    ?partition_by:a_expr list ->
    ?order_by:(a_expr * [ `ASC | `DESC ]) list ->
    ('n, 't number) expr ->
    (non_null, 't number) expr

  val uniq :
    ?partition_by:a_expr list ->
    ?order_by:(a_expr * [ `ASC | `DESC ]) list ->
    ('n, _) expr ->
    (non_null, int number) expr
end

module Parser : module type of Parser
module Lexer : module type of Lexer
module Syntax : module type of Syntax
module Printer : module type of Printer
module Loc : module type of Loc
