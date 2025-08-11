(** DSL for SQL queries generation. *)

type null = [ `not_null | `null ]
type non_null = [ `not_null ]
type 'a nullable = 'a constraint 'a = [< null ]

(** Used for "subtyping" of numeric types. *)
type 'a number = private A_number

type ('null, 'a) array = private A_array
type ('null, 'a) typ = private A_typ
type ('x, 'y) tuple2 = private A_tuple2
type date = private Date
type datetime = private DateTime

type (+'null, +'typ) expr
(** An SQL expression. *)

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

(** Used in cases where we don't care about the type of the expression (GROUP
    BY, ORDER BY, etc.). *)
type a_expr = A_expr : _ expr -> a_expr

val int : int -> (non_null, int number) expr
val string : string -> (non_null, string) expr
val bool : bool -> (non_null, bool) expr
val float : float -> (non_null, float number) expr
val null : (null, 'a) expr

val lambda :
  string ->
  (('pn, 'pa) expr -> ('n, 'a) expr) ->
  (non_null, ('pn, 'pa) expr -> ('n, 'a) expr) expr

val array : ('n, 'a) expr list -> (non_null, ('n, 'a) array) expr

type 'a in_rhs =
  | In_query : < _1 : (_, 'a) expr > scope select -> 'a in_rhs
  | In_array : (_, (_, 'a) array) expr -> 'a in_rhs

val in_ : (_, 'a) expr -> 'a in_rhs -> (non_null, bool) expr

val unsafe : string -> _ expr
(** Inject a string into an expression without any checks. This is unsafe to do,
    if string comes from user input (= SQL injection). *)

val unsafe_concat : a_expr list -> _ expr

val select :
  from:'from from ->
  ?prewhere:('from -> (_, bool) expr) ->
  ?where:('from -> (_, bool) expr) ->
  ?qualify:('from -> (_, bool) expr) ->
  ?group_by:('from -> a_expr list) ->
  ?having:('from -> (_, bool) expr) ->
  ?order_by:('from -> (a_expr * [ `ASC | `DESC ]) list) ->
  ?limit:('from -> (_, int number) expr) ->
  ?offset:('from -> (_, int number) expr) ->
  ?settings:(string * [ `Int of int | `String of string | `Bool of bool ]) list ->
  select:('from -> 'select) ->
  unit ->
  'select scope select

val union : 'a scope select -> 'a scope select -> 'a scope select
(** UNION of two SELECT queries. *)

val from : 'a scope from_one -> 'a scope from

val join :
  'a from ->
  'b scope from_one ->
  on:('a * 'b scope -> (_, bool) expr) ->
  ('a * 'b scope) from

val left_join :
  ?optional:bool ->
  'a from ->
  'b scope from_one ->
  on:('a * 'b scope -> (_, bool) expr) ->
  ('a * 'b nullable_scope) from

val from_select :
  ?cluster_name:string -> alias:string -> 'a scope select -> 'a scope from_one

val from_table :
  db:string ->
  table:string ->
  (alias:string -> 'a) ->
  final:bool ->
  alias:string ->
  'a scope from_one

module Expr : sig
  val assumeNotNull : ([< null ] nullable, 'b) expr -> (non_null, 'a) expr
  val toNullable : (_, 'a) expr -> (null nullable, 'a) expr
  val coalesce : ('b, 'a) expr -> ('n, 'a) expr -> ('n, 'a) expr
  val eq : ('n, 'a) expr -> ('n, 'a) expr -> ('n, bool) expr
  val ( = ) : ('n, 'a) expr -> ('n, 'a) expr -> ('n, bool) expr
  val gt : ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, bool) expr
  val ( > ) : ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, bool) expr
  val lt : ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, bool) expr
  val ( < ) : ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, bool) expr
  val ge : ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, bool) expr
  val ( >= ) : ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, bool) expr
  val le : ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, bool) expr
  val ( <= ) : ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, bool) expr

  val plus :
    ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr

  val ( + ) :
    ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr

  val minus :
    ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr

  val ( - ) :
    ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr

  val multiply :
    ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr

  val ( * ) :
    ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr

  val divide :
    ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr

  val ( / ) :
    ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr

  val ( && ) : ('n, bool) expr -> ('n, bool) expr -> ('n, bool) expr
  val ( || ) : ('n, bool) expr -> ('n, bool) expr -> ('n, bool) expr
  val not_ : ('n, bool) expr -> ('n, bool) expr
  val toDate : ('n, _ number) expr -> ('n, date) expr
  val if_ : ('n, bool) expr -> ('n, 'a) expr -> ('n, 'a) expr -> ('n, 'a) expr

  val arrayFilter :
    (non_null, ('n, 'a) expr -> (_, bool) expr) expr ->
    ('m, ('n, 'a) array) expr ->
    ('m, ('n, 'a) array) expr

  val length : ('n, _ array) expr -> ('n, int number) expr

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

type json =
  [ `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of json list
  | `Null
  | `String of string ]
(** JSON value (compatible with Yojson.Basic.t) *)

module Row : sig
  type 'a t
  (** Represents a row to query (both expressions and parser). *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  (** Map result of a row.

      [let+ x = P in E] parses with [P] and then makes result [x] available in
      an expression [E]. *)

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  (** Combine two rows.

      [let+ x = P1 and+ y = P2 in E] parses with [P1] and [P2] and makes results
      available as a tuple [x] and [y] in an expression [E]. *)

  val return : 'a -> 'a t
  (** Return a constant value. *)

  val string : (non_null, string) expr -> string t
  val string_opt : ([< null ], string) expr -> string option t
  val bool : (non_null, bool) expr -> bool t
  val bool_opt : ([< null ], bool) expr -> bool option t
  val int : (non_null, int number) expr -> int t
  val int_opt : ([< null ], int number) expr -> int option t
  val int64 : (non_null, int64 number) expr -> int64 t
  val int64_opt : ([< null ], int64 number) expr -> int64 option t
  val float : (non_null, float number) expr -> float t
  val float_opt : ([< null ], float number) expr -> float option t

  exception Parse_error of string

  val parse : 'a t -> json list -> 'a
  (** Parse a row from a JSON list.

      Raises [Parse_error] if the row is not compatible with the parser. *)
end

val query :
  'a scope select -> ('a scope -> 'row Row.t) -> string * (json list -> 'row)
(** [query select f] queries the SELECT query by defining a set of columns of
    interest (along with the parser).

    The result is the SQL string and a function to parse a single result row.
    The parse function raises [Row.Parse_error] in case of incorrect row being
    supplied. *)
