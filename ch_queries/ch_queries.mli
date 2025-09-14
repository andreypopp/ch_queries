(** DSL for SQL queries generation. *)

type null = [ `not_null | `null ]
type non_null = [ `not_null ]
type 'a nullable = 'a constraint 'a = [< null ]

(** Used for "subtyping" of numeric types. *)
type 'a number = private A_number

type 'a timestamp = private A_timestamp
type ('null, 'a) array = private A_array
type ('nullk, 'k, 'nullv, 'v) map = private A_map
type ('null, 'a) typ = private A_typ
type ('x, 'y) tuple2 = private A_tuple2
type date = private Date
type datetime = private DateTime
type datetime64 = private DateTime64
type interval = private Interval

type (+'null, +'typ) expr
(** An SQL expression. *)

(** Used in cases where we don't care about the type of the expression (GROUP
    BY, ORDER BY, etc.). *)
type a_expr = A_expr : _ expr -> a_expr

type 'a scope =
  < query' :
      'n 'e.
      ('a -> ('n, 'e) expr) -> ('n, 'e) expr * (force:bool -> ('n, 'e) expr)
  ; query : 'n 'e. ('a -> ('n, 'e) expr) -> ('n, 'e) expr
  ; query_many : ('a -> a_expr list) -> a_expr list >
(** Represents a table's or a subquery's scope. *)

and 'a nullable_scope =
  < query' :
      'n 'e.
      ('a -> ('n, 'e) expr) -> ('n, 'e) expr * (force:bool -> (null, 'e) expr)
  ; query : 'n 'e. ('a -> ('n, 'e) expr) -> (null, 'e) expr
  ; query_many : ('a -> a_expr list) -> a_expr list >
(** Represents a table's or a subquery's scope when one is used on the right
    side of a LEFT JOIN. *)

type 'scope select
(** SELECT query. *)

type 'a from_one
(** Represents a single table or subquery in the FROM clause. *)

type 'a from
(** Represents the FROM clause of a query, which can contain multiple tables or
    subqueries. *)

val int : int -> (non_null, int number) expr
val string : string -> (non_null, string) expr
val bool : bool -> (non_null, bool) expr
val float : float -> (non_null, float number) expr
val null : (null, 'a) expr

val interval :
  int ->
  [< `YEAR | `MONTH | `WEEK | `DAY | `HOUR | `MINUTE | `SECOND ] ->
  (non_null, interval) expr

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

val grouping_sets : a_expr list list -> _ expr
(** GROUPING SETS for GROUP BY clause. *)

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

val map_from_scope : 'x from -> ('x -> 'y) -> 'y from

val from_select :
  ?cluster_name:string -> alias:string -> 'a scope select -> 'a scope from_one

val from_table :
  db:string ->
  table:string ->
  (alias:string -> 'a) ->
  final:bool ->
  alias:string ->
  'a scope from_one

val with_cte :
  ?materialized:bool ->
  alias:string ->
  'a scope select ->
  ((alias:string -> 'a scope from_one) -> 'b select) ->
  'b select

module Expr : sig
  (** {1 Regular functions} *)

  (** {2 Arithmetic} *)

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

  val negate : ('n, 'a number) expr -> ('n, 'a number) expr
  val abs : ('n, 'a number) expr -> ('n, 'a number) expr

  val intDiv :
    ('n, _ number) expr -> ('n, _ number) expr -> ('n, int number) expr

  val modulo :
    ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr

  (** {2 Arrays} *)

  val arrayElement :
    (non_null, ('n, 'a) array) expr ->
    (non_null, int number) expr ->
    ('n, 'a) expr

  val arrayElementOrNull :
    (non_null, ('n, 'a) array) expr ->
    (non_null, int number) expr ->
    ('m, 'a) expr

  val arrayFilter :
    (non_null, ('n, 'a) expr -> (_, bool) expr) expr ->
    ('m, ('n, 'a) array) expr ->
    ('m, ('n, 'a) array) expr

  val length : ('n, _ array) expr -> ('n, int number) expr

  (** {2 Conditional} *)

  val if_ : ('n, bool) expr -> ('n, 'a) expr -> ('n, 'a) expr -> ('n, 'a) expr

  (** {2 Comparisons} *)

  val equals : ('n, 'a) expr -> ('n, 'a) expr -> ('n, bool) expr
  val ( = ) : ('n, 'a) expr -> ('n, 'a) expr -> ('n, bool) expr
  val notEquals : ('n, 'a) expr -> ('n, 'a) expr -> ('n, bool) expr
  val ( != ) : ('n, 'a) expr -> ('n, 'a) expr -> ('n, bool) expr
  val ( <> ) : ('n, 'a) expr -> ('n, 'a) expr -> ('n, bool) expr
  val greater : ('n, 'a number) expr -> ('n, 'b number) expr -> ('n, bool) expr
  val ( > ) : ('n, 'a number) expr -> ('n, 'b number) expr -> ('n, bool) expr
  val less : ('n, 'a number) expr -> ('n, 'b number) expr -> ('n, bool) expr
  val ( < ) : ('n, 'a number) expr -> ('n, 'b number) expr -> ('n, bool) expr

  val greaterOrEquals :
    ('n, 'a number) expr -> ('n, 'b number) expr -> ('n, bool) expr

  val ( >= ) : ('n, 'a number) expr -> ('n, 'b number) expr -> ('n, bool) expr

  val lessOrEquals :
    ('n, 'a number) expr -> ('n, 'b number) expr -> ('n, bool) expr

  val ( <= ) : ('n, 'a number) expr -> ('n, 'b number) expr -> ('n, bool) expr

  (** {2 Dates and times} *)

  val toDate : ('n, _) expr -> ('n, date timestamp) expr
  val toDateTime : ('n, _) expr -> ('n, datetime timestamp) expr
  val now : unit -> (non_null, datetime timestamp) expr
  val today : unit -> (non_null, date timestamp) expr
  val yesterday : unit -> (non_null, date timestamp) expr

  val addDate :
    ('n, 'a timestamp) expr -> ('n, interval) expr -> ('n, 'a timestamp) expr

  val addDays :
    ('n, 'a timestamp) expr -> ('n, int number) expr -> ('n, 'a timestamp) expr

  val addHours :
    ('n, 'a timestamp) expr -> ('n, int number) expr -> ('n, 'a timestamp) expr

  val addInterval :
    ('n, 'a timestamp) expr -> ('n, interval) expr -> ('n, 'a timestamp) expr

  val addMinutes :
    ('n, 'a timestamp) expr -> ('n, int number) expr -> ('n, 'a timestamp) expr

  val addMonths :
    ('n, 'a timestamp) expr -> ('n, int number) expr -> ('n, 'a timestamp) expr

  val addSeconds :
    ('n, 'a timestamp) expr -> ('n, int number) expr -> ('n, 'a timestamp) expr

  val addWeeks :
    ('n, 'a timestamp) expr -> ('n, int number) expr -> ('n, 'a timestamp) expr

  val addYears :
    ('n, 'a timestamp) expr -> ('n, int number) expr -> ('n, 'a timestamp) expr

  val subDate :
    ('n, 'a timestamp) expr -> ('n, interval) expr -> ('n, 'a timestamp) expr

  val subtractDays :
    ('n, 'a timestamp) expr -> ('n, int number) expr -> ('n, 'a timestamp) expr

  val subtractHours :
    ('n, 'a timestamp) expr -> ('n, int number) expr -> ('n, 'a timestamp) expr

  val subtractMinutes :
    ('n, 'a timestamp) expr -> ('n, int number) expr -> ('n, 'a timestamp) expr

  val subtractMonths :
    ('n, 'a timestamp) expr -> ('n, int number) expr -> ('n, 'a timestamp) expr

  val subtractSeconds :
    ('n, 'a timestamp) expr -> ('n, int number) expr -> ('n, 'a timestamp) expr

  val subtractWeeks :
    ('n, 'a timestamp) expr -> ('n, int number) expr -> ('n, 'a timestamp) expr

  val subtractYears :
    ('n, 'a timestamp) expr -> ('n, int number) expr -> ('n, 'a timestamp) expr

  val toYYYYMM : ('n, _ timestamp) expr -> ('n, int number) expr
  val toYYYYMMDD : ('n, _ timestamp) expr -> ('n, int number) expr
  val toStartOfYear : ('n, 'a timestamp) expr -> ('n, 'a timestamp) expr
  val toStartOfMonth : ('n, 'a timestamp) expr -> ('n, 'a timestamp) expr
  val toStartOfWeek : ('n, 'a timestamp) expr -> ('n, 'a timestamp) expr
  val toStartOfDay : ('n, 'a timestamp) expr -> ('n, 'a timestamp) expr
  val toStartOfHour : ('n, 'a timestamp) expr -> ('n, 'a timestamp) expr
  val toStartOfMinute : ('n, 'a timestamp) expr -> ('n, 'a timestamp) expr
  val fromUnixTimestamp : ('n, int number) expr -> ('n, datetime timestamp) expr

  (** {2 Logical} *)

  val ( && ) : ('n, bool) expr -> ('n, bool) expr -> ('n, bool) expr
  val ( || ) : ('n, bool) expr -> ('n, bool) expr -> ('n, bool) expr
  val not_ : ('n, bool) expr -> ('n, bool) expr

  (** {2 Map}*)

  val map :
    (('nk, 'k) expr * ('nv, 'v) expr) list ->
    (non_null, ('nk, 'k, 'nv, 'v) map) expr

  val map_get :
    (non_null, ('nk, 'k, 'nv, 'v) map) expr ->
    (non_null, 'k) expr ->
    ('nv, 'v) expr
  (** compiles to clickhouse function {!arrayElement}, but is named differently
      to allow its use on maps. *)

  (** {2 Nullable} *)

  val assumeNotNull : ([< null ] nullable, 'b) expr -> (non_null, 'a) expr
  val coalesce : ('b, 'a) expr -> ('n, 'a) expr -> ('n, 'a) expr
  val toNullable : (_, 'a) expr -> (null nullable, 'a) expr

  (** {2 String} *)

  val empty : ('n, string) expr -> ('n, bool) expr
  val notEmpty : ('n, string) expr -> ('n, bool) expr

  (** {2 String replacement} *)

  val replaceOne :
    ('n, string) expr ->
    (non_null, string) expr ->
    (non_null, string) expr ->
    ('n, string) expr

  (** {2 String search} *)

  val like : ('n, string) expr -> (non_null, string) expr -> ('n, bool) expr

  (** {2 Type conversions} *)

  val toInt64 : ('n, string) expr -> ('n, int number) expr
  val toUInt64 : ('n, string) expr -> ('n, int number) expr

  (** {1 Aggregate functions} *)

  val avg :
    ?partition_by:a_expr list ->
    ?order_by:(a_expr * [ `ASC | `DESC ]) list ->
    ('n, 't number) expr ->
    (non_null, 't number) expr

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

  type ('null, 'sql_type, 'ocaml_type) parser

  val col : ('n, 's) expr -> ('n, 's, 'o) parser -> 'o t
  val ignore : ('n, 'a) expr -> unit t

  (* Parsers *)

  val string : (non_null, string, string) parser
  val bool : (non_null, bool, bool) parser
  val int : (non_null, int number, int) parser
  val int64 : (non_null, int64 number, int64) parser
  val uint64 : (non_null, int64 number, Unsigned.uint64) parser
  val float : (non_null, float number, float) parser
  val date : (non_null, date timestamp, float) parser
  val datetime : (non_null, datetime timestamp, float) parser
  val any : (_, _, json) parser
  val nullable : ('n, 's, 'o) parser -> (null, 's, 'o option) parser
  val array : ('n, 's, 'o) parser -> (non_null, ('n, 's) array, 'o list) parser

  val map :
    ('nk, 'sk, 'ok) parser ->
    ('nv, 'sv, 'ov) parser ->
    (non_null, ('nk, 'sk, 'nv, 'sv) map, ('ok * 'ov) list) parser

  exception Parse_error of json option * string

  val parse : 'a t -> json list -> 'a
  (** Parse a row from a JSON list.

      Raises [Parse_error] if the row is not compatible with the parser. *)
end

val query :
  'a scope select ->
  (< q : 'a scope > -> 'row Row.t) ->
  string * (json list -> 'row)
(** [query select f] queries the SELECT query by defining a set of columns of
    interest (along with the parser).

    The result is the SQL string and a function to parse a single result row.
    The parse function raises [Row.Parse_error] in case of incorrect row being
    supplied. *)
