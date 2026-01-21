(** DSL for SQL queries generation. *)

type uint64 = Unsigned.uint64

(** Nullability markers. *)

type null = [ `not_null | `null ]
type non_null = [ `not_null ]
type 'a nullable = 'a constraint 'a = [< null ]

(** Types tagged with this are comparable. *)
type 'a comparable = private A_comparable

type 'a number0 = private A_number

type 'a number = 'a number0 comparable
(** Used for "subtyping" of numeric types. *)

type 'a timestamp0 = private A_timestamp
type 'a timestamp = 'a timestamp0 comparable
type ('null, 'a) array = private A_array
type ('nullk, 'k, 'nullv, 'v) map = private A_map
type ('null, 'a) typ = private A_typ
type ('x, 'y) tuple2 = private A_tuple2
type ('x, 'y, 'z) tuple3 = private A_tuple3
type ('x, 'y, 'z, 'w) tuple4 = private A_tuple4
type date0 = private Date
type date = date0 timestamp
type datetime0 = private DateTime
type datetime = datetime0 timestamp
type datetime640 = private DateTime64
type datetime64 = datetime640 timestamp
type interval = private Interval
type ('n, 'a) agg_state = private Agg_state

type (+'null, +'typ) expr
(** An SQL expression. *)

(** Used in cases where we don't care about the type of the expression (GROUP
    BY, ORDER BY, etc.). *)
type a_expr = A_expr : _ expr -> a_expr

type fill = {
  fill_from : a_expr option;
  fill_to : a_expr option;
  fill_step : a_expr option;
  fill_interpolate : (string * a_expr option) list;
}
(** Fill specification for ORDER BY WITH FILL clause. *)

type an_order_by = a_expr * [ `ASC | `DESC ] * fill option
(** ORDER BY item with optional fill specification. *)

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

module Dict : sig
  type ('keys, 'values) t = {
    db : string;
    table : string;
    keys : 'keys;
    values : 'values scope;
  }

  val make :
    db:string ->
    table:string ->
    keys:'keys ->
    values:'values ->
    ('keys, 'values) t

  val unsafe_value : string -> ('n, 'a) expr
end

type 'scope select
(** SELECT query. *)

type 'a from_one
(** Represents a single table or subquery in the FROM clause. *)

type 'a from
(** Represents the FROM clause of a query, which can contain multiple tables or
    subqueries. *)

val int : int -> (non_null, int number) expr
val int64 : int64 -> (non_null, int64 number) expr
val uint64 : uint64 -> (non_null, uint64 number) expr
val string : string -> (non_null, string) expr
val bool : bool -> (non_null, bool) expr
val float : float -> (non_null, float number) expr
val null : (null, 'a) expr
val date : float -> (non_null, date) expr
val datetime : float -> (non_null, datetime) expr

val interval :
  int ->
  [< `YEAR | `MONTH | `WEEK | `DAY | `HOUR | `MINUTE | `SECOND ] ->
  (non_null, interval) expr

val lambda :
  string ->
  (('pn, 'pa) expr -> ('n, 'a) expr) ->
  (non_null, ('pn, 'pa) expr -> ('n, 'a) expr) expr

val lambda2 :
  string ->
  string ->
  (('pn1, 'pa1) expr -> ('pn2, 'pa2) expr -> ('n, 'a) expr) ->
  (non_null, ('pn1, 'pa1) expr -> ('pn2, 'pa2) expr -> ('n, 'a) expr) expr

val array : ('n, 'a) expr list -> (non_null, ('n, 'a) array) expr

type 'a in_rhs =
  | In_query : < _1 : (_, 'a) expr > scope select -> 'a in_rhs
  | In_array : (_, (_, 'a) array) expr -> 'a in_rhs

val in_ : (_, 'a) expr -> 'a in_rhs -> (non_null, bool) expr

val unsafe_col : string -> string -> _ expr
(** Construct and unsafe column expression. *)

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
  ?order_by:('from -> an_order_by list) ->
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

type a_field = A_field : _ expr * string -> a_field

val select_syntax :
  from:'from from ->
  ?prewhere:('from -> (_, bool) expr) ->
  ?where:('from -> (_, bool) expr) ->
  ?qualify:('from -> (_, bool) expr) ->
  ?group_by:('from -> a_expr list) ->
  ?having:('from -> (_, bool) expr) ->
  ?order_by:('from -> an_order_by list) ->
  ?limit:('from -> (_, int number) expr) ->
  ?offset:('from -> (_, int number) expr) ->
  ?settings:(string * [ `Int of int | `String of string | `Bool of bool ]) list ->
  select:('from -> a_field list) ->
  unit ->
  Ch_queries_syntax.Syntax.query
(** Same as [select] but generates syntax AST which select all the fields. *)

val expr_to_syntax : _ expr -> Ch_queries_syntax.Syntax.expr
(** Convert an expression to syntax AST. *)

val expr_to_string : _ expr -> string
(** Convert an expression to string. *)

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
    ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, float number) expr

  val ( / ) :
    ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, float number) expr

  val negate : ('n, 'a number) expr -> ('n, 'a number) expr
  val abs : ('n, 'a number) expr -> ('n, 'a number) expr

  val intDiv :
    ('n, _ number) expr -> ('n, _ number) expr -> ('n, int number) expr

  val intDivOrNull :
    ('n, _ number) expr -> ('n, _ number) expr -> (null, int number) expr
  (** Same as [intDiv] but returns NULL when dividing by zero or when dividing a minimal negative number by minus one. *)

  val intDivOrZero :
    ('n, _ number) expr -> ('n, _ number) expr -> ('n, int number) expr
  (** Same as [intDiv] but returns zero when dividing by zero or when dividing a minimal negative number by minus one. *)

  val modulo :
    ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr

  val moduloOrNull :
    ('n, 'a number) expr -> ('n, 'a number) expr -> (null, 'a number) expr
  (** Same as [modulo] but returns NULL when the divisor is zero. *)

  val moduloOrZero :
    ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr
  (** Same as [modulo] but returns zero when the divisor is zero. *)

  val positiveModulo :
    ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr
  (** Calculates the remainder when dividing x by y. Similar to [modulo] except
      that positiveModulo always returns a non-negative number. *)

  val positiveModuloOrNull :
    ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr
  (** Similar to [positiveModulo] except that it returns NULL when the divisor
      is zero. *)

  val divideDecimal :
    ?result_scale:('n, int number) expr ->
    ('n, 'a number) expr ->
    ('n, 'b number) expr ->
    ('n, 'c number) expr

  val multiplyDecimal :
    ?result_scale:('n, int number) expr ->
    ('n, 'a number) expr ->
    ('n, 'b number) expr ->
    ('n, 'c number) expr
  (** Performs multiplication on two decimals with optional result scale.
      Result value will be of type Decimal256. *)

  val divideOrNull :
    ('n, 'a number) expr -> ('n, 'a number) expr -> (null, float number) expr
  (** Same as [divide] but returns NULL when dividing by zero. *)

  val gcd :
    ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr
  (** Returns the greatest common divisor of two values. *)

  val lcm :
    ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr
  (** Returns the least common multiple of two values. *)

  val max2 :
    ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr
  (** Returns the bigger of two numeric values. *)

  val min2 :
    ('n, 'a number) expr -> ('n, 'a number) expr -> ('n, 'a number) expr
  (** Returns the smaller of two numeric values. *)

  val midpoint : ('n, 'a) expr list -> ('n, 'a) expr
  (** Returns the average value of the provided arguments. Supports numerical and temporal types. *)

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

  val arrayAll :
    (non_null, ('n, 'a) expr -> (_, bool) expr) expr ->
    ('m, ('n, 'a) array) expr list ->
    (non_null, int number) expr

  val arrayAvg :
    (non_null, ('n, 'a) expr -> (_, _ number) expr) expr ->
    ('m, ('n, 'a) array) expr list ->
    (non_null, float number) expr
  (** [arrayAvg func arrays] returns the average of elements of the lambda
      results. The [func] is applied to corresponding elements from all arrays. *)

  val arrayCount :
    ?f:(non_null, ('n, 'a) expr -> (_, bool) expr) expr ->
    ('m, ('n, 'a) array) expr list ->
    (non_null, int number) expr
  (** [arrayCount ?f arrays] returns the number of elements for which [f]
      returns true. If [f] is not specified, returns the number of non-zero
      elements in the array. The [f] is applied to corresponding elements from
      all arrays. *)

  val arrayCumSum :
    ?f:(non_null, ('n, 'a) expr -> (_, _ number) expr) expr ->
    ('m, ('n, 'a) array) expr list ->
    ('m, ('n, 'a) array) expr
  (** [arrayCumSum ?f arrays] returns an array of the partial (running) sums of
      the elements in the source array. If [f] is specified, the sum is computed
      from applying the lambda to the array elements at each position. *)

  val arrayCumSumNonNegative :
    ?f:(non_null, ('n, 'a) expr -> (_, _ number) expr) expr ->
    ('m, ('n, 'a) array) expr list ->
    ('m, ('n, 'a) array) expr
  (** [arrayCumSumNonNegative ?f arrays] returns an array of the partial
      (running) sums of the elements in the source array, replacing any negative
      running sum with zero. If [f] is specified, the sum is computed from
      applying the lambda to the array elements at each position. *)

  val length : ('n, _ array) expr -> ('n, int number) expr

  val arrayJoin : ('n, ('m, 'a) array) expr -> ('m, 'a) expr
  (** [arrayJoin arr] unfolds the array into multiple rows - one per element.
      This is a special function that affects all query clauses. *)

  val arrayCompact : ('n, ('m, 'a) array) expr -> ('n, ('m, 'a) array) expr
  (** [arrayCompact arr] removes consecutive duplicate elements from an array.
      The order of values in the resulting array is determined by the order in
      the source array. *)

  val arrayConcat : ('n, ('m, 'a) array) expr list -> ('n, ('m, 'a) array) expr
  (** [arrayConcat arrays] combines arrays passed as arguments. *)

  val arrayDifference : ('n, ('m, 'a) array) expr -> ('n, ('m, 'a) array) expr
  (** [arrayDifference arr] calculates an array of differences between adjacent
      array elements. The first element will be 0, the second [arr\[1\] - arr\[0\]],
      the third [arr\[2\] - arr\[1\]], etc. *)

  val arrayDistinct : ('n, ('m, 'a) array) expr -> ('n, ('m, 'a) array) expr
  (** [arrayDistinct arr] returns an array containing only the distinct elements. *)

  val arrayDotProduct :
    ('n, ('m, 'a number) array) expr ->
    ('n, ('m, 'b number) array) expr ->
    ('n, 'c number) expr
  (** [arrayDotProduct v1 v2] returns the dot product of two arrays. The sizes
      of the two vectors must be equal. *)

  val arrayEnumerate : ('n, ('m, 'a) array) expr -> ('n, ('m, int number) array) expr
  (** [arrayEnumerate arr] returns the array [1, 2, 3, ..., length(arr)]. *)

  val arrayEnumerateDense : ('n, ('m, 'a) array) expr -> ('n, ('m, int number) array) expr
  (** [arrayEnumerateDense arr] returns an array of the same size as the source
      array, indicating where each element first appears in the source array. *)

  val arrayEnumerateDenseRanked :
    ('n, _ number) expr ->
    ('n, ('m, 'a) array) expr ->
    ('n, _ number) expr ->
    ('n, ('m, int number) array) expr
  (** [arrayEnumerateDenseRanked clear_depth arr max_array_depth] returns an
      array the same size as the source array, indicating where each element
      first appears in the source array. It allows for enumeration of a
      multidimensional array with the ability to specify how deep to look inside
      the array. *)

  val arrayEnumerateUniq :
    ('n, ('m, 'a) array) expr list ->
    ('n, ('m, int number) array) expr
  (** [arrayEnumerateUniq arrs] returns an array the same size as the source
      array, indicating for each element what its position is among elements
      with the same value. When multiple arrays are passed, uniqueness is
      considered for tuples of elements in the same positions in all arrays. *)

  val arrayEnumerateUniqRanked :
    ('n, _ number) expr ->
    ('n, ('m, 'a) array) expr ->
    ('n, _ number) expr ->
    ('n, ('m, int number) array) expr
  (** [arrayEnumerateUniqRanked clear_depth arr max_array_depth] returns an
      array the same size as the source array, indicating for each element what
      its position is among elements with the same value. It allows for
      enumeration of a multidimensional array with the ability to specify how
      deep to look inside the array. *)

  val arrayExcept :
    ('n, ('m, 'a) array) expr ->
    ('n, ('m, 'a) array) expr ->
    ('n, ('m, 'a) array) expr
  (** [arrayExcept source except] returns an array containing elements from
      [source] that are not present in [except], preserving the original order. *)

  val arrayExists :
    (non_null, ('n, 'a) expr -> (_, bool) expr) expr ->
    ('m, ('n, 'a) array) expr list ->
    (non_null, int number) expr
  (** [arrayExists func arrays] returns 1 if there is at least one element in
      the source array for which [func] returns true, otherwise returns 0. When
      multiple arrays are passed, [func] operates on corresponding elements from
      all arrays. *)

  val arrayFill :
    (non_null, ('n, 'a) expr -> (_, bool) expr) expr ->
    ('m, ('n, 'a) array) expr list ->
    ('m, ('n, 'a) array) expr
  (** [arrayFill func arrays] scans through the source array from the first to
      last element and replaces each position where the [func] returns false
      with the value at the previous position (first element is always
      preserved). When multiple arrays are passed, [func] operates on
      corresponding elements from all arrays, but only the first array is
      modified. *)

  val arrayFirst :
    (non_null, ('n, 'a) expr -> (_, bool) expr) expr ->
    ('m, ('n, 'a) array) expr list ->
    ('n, 'a) expr
  (** [arrayFirst func arrays] returns the first element in the source array
      for which [func] returns true, otherwise it returns a default value.
      When multiple arrays are passed, [func] operates on corresponding
      elements from all arrays. *)

  val arrayFirstIndex :
    (non_null, ('n, 'a) expr -> (_, bool) expr) expr ->
    ('m, ('n, 'a) array) expr list ->
    (non_null, int number) expr
  (** [arrayFirstIndex func arrays] returns the index (1-based) of the first
      element in the source array for which [func] returns true, otherwise
      returns 0. When multiple arrays are passed, [func] operates on
      corresponding elements from all arrays. *)

  val arrayFirstOrNull :
    (non_null, ('n, 'a) expr -> (_, bool) expr) expr ->
    ('m, ('n, 'a) array) expr list ->
    (null, 'a) expr
  (** [arrayFirstOrNull func arrays] returns the first element in the source
      array for which [func] returns true, otherwise returns NULL. When
      multiple arrays are passed, [func] operates on corresponding elements
      from all arrays. *)

  val arrayFlatten : ('n, ('m, ('o, 'a) array) array) expr -> ('n, ('o, 'a) array) expr

  val arrayLast :
    (non_null, ('n, 'a) expr -> (_, bool) expr) expr ->
    ('m, ('n, 'a) array) expr list ->
    ('n, 'a) expr
  (** [arrayLast func arrays] returns the last element in the source array
      for which [func] returns true, otherwise it returns a default value.
      When multiple arrays are passed, [func] operates on corresponding
      elements from all arrays. *)

  val arrayLastIndex :
    (non_null, ('n, 'a) expr -> (_, bool) expr) expr ->
    ('m, ('n, 'a) array) expr list ->
    (non_null, int number) expr
  (** [arrayLastIndex func arrays] returns the index (1-based) of the last
      element in the source array for which [func] returns true, otherwise
      returns 0. When multiple arrays are passed, [func] operates on
      corresponding elements from all arrays. *)

  val arrayLastOrNull :
    (non_null, ('n, 'a) expr -> (_, bool) expr) expr ->
    ('m, ('n, 'a) array) expr list ->
    (null, 'a) expr
  (** [arrayLastOrNull func arrays] returns the last element in the source
      array for which [func] returns true, otherwise returns NULL. When
      multiple arrays are passed, [func] operates on corresponding elements
      from all arrays. *)

  val arrayFold :
    (non_null, ('na, 'acc) expr -> ('n, 'a) expr -> ('na, 'acc) expr) expr ->
    ('m, ('n, 'a) array) expr list ->
    ('na, 'acc) expr ->
    ('na, 'acc) expr
  (** [arrayFold func arrays acc] applies [func] to one or more equally-sized
      arrays and collects the result in an accumulator. The [func] is a lambda
      function that takes an accumulator and array values, returning the new
      accumulator value. [acc] is the initial accumulator value. *)

  val arrayIntersect : ('n, ('m, 'a) array) expr list -> ('n, ('m, 'a) array) expr
  (** [arrayIntersect arrays] returns an array with elements that are present in
      all source arrays. The result contains only unique values. *)

  val arrayJaccardIndex :
    ('n, ('m, 'a) array) expr ->
    ('n, ('m, 'a) array) expr ->
    ('n, float number) expr
  (** [arrayJaccardIndex arr_x arr_y] returns the Jaccard index of two arrays.
      The Jaccard index is the ratio of the size of the intersection to the size
      of the union of the two arrays. *)

  val arrayLevenshteinDistance :
    ('n, ('m, 'a) array) expr ->
    ('n, ('m, 'a) array) expr ->
    ('n, float number) expr
  (** [arrayLevenshteinDistance arr_from arr_to] calculates the Levenshtein
      distance between two arrays. *)

  val arrayLevenshteinDistanceWeighted :
    ('n, ('m, 'a) array) expr ->
    ('n, ('m, 'a) array) expr ->
    ('n, ('m, 'w number) array) expr ->
    ('n, ('m, 'w number) array) expr ->
    ('n, float number) expr
  (** [arrayLevenshteinDistanceWeighted arr_from arr_to from_weights to_weights]
      calculates the Levenshtein distance between two arrays with custom weights
      for each element. *)

  (** {2 Conditional} *)

  val if_ : (_, bool) expr -> ('n, 'a) expr -> ('n, 'a) expr -> ('n, 'a) expr

  val multiIf :
    ((_, bool) expr * ('n, 'a) expr) list ->
    else_:('n, 'a) expr ->
    ('n, 'a) expr

  (** {2 Equality} *)

  val equals : ('n, 'a) expr -> ('n, 'a) expr -> ('n, bool) expr
  val ( = ) : ('n, 'a) expr -> ('n, 'a) expr -> ('n, bool) expr
  val notEquals : ('n, 'a) expr -> ('n, 'a) expr -> ('n, bool) expr
  val ( != ) : ('n, 'a) expr -> ('n, 'a) expr -> ('n, bool) expr
  val ( <> ) : ('n, 'a) expr -> ('n, 'a) expr -> ('n, bool) expr

  (** {2 Comparison} *)

  val greater :
    ('n, 'a comparable) expr -> ('n, 'b comparable) expr -> ('n, bool) expr

  val ( > ) :
    ('n, 'a comparable) expr -> ('n, 'b comparable) expr -> ('n, bool) expr

  val less :
    ('n, 'a comparable) expr -> ('n, 'b comparable) expr -> ('n, bool) expr

  val ( < ) :
    ('n, 'a comparable) expr -> ('n, 'b comparable) expr -> ('n, bool) expr

  val greaterOrEquals :
    ('n, 'a comparable) expr -> ('n, 'b comparable) expr -> ('n, bool) expr

  val ( >= ) :
    ('n, 'a comparable) expr -> ('n, 'b comparable) expr -> ('n, bool) expr

  val lessOrEquals :
    ('n, 'a comparable) expr -> ('n, 'b comparable) expr -> ('n, bool) expr

  val ( <= ) :
    ('n, 'a comparable) expr -> ('n, 'b comparable) expr -> ('n, bool) expr

  (** {2 Dates and times} *)

  val toDate : ('n, _) expr -> ('n, date) expr
  val toDateTime : ('n, _) expr -> ('n, datetime) expr
  val now : unit -> (non_null, datetime) expr
  val now64 : unit -> (non_null, datetime64) expr
  val today : unit -> (non_null, date) expr
  val yesterday : unit -> (non_null, date) expr

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

  val subtractInterval :
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

  val toStartOfInterval :
    ('n, 'a timestamp) expr ->
    (non_null, interval) expr ->
    ('n, 'a timestamp) expr

  val fromUnixTimestamp : ('n, int number) expr -> ('n, datetime) expr
  val toIntervalMinute : ('n, int number) expr -> ('n, interval) expr
  val toIntervalHour : ('n, int number) expr -> ('n, interval) expr
  val toIntervalDay : ('n, int number) expr -> ('n, interval) expr
  val toIntervalWeek : ('n, int number) expr -> ('n, interval) expr
  val toIntervalMonth : ('n, int number) expr -> ('n, interval) expr
  val toIntervalYear : ('n, int number) expr -> ('n, interval) expr

  (** {2 Logical} *)

  val ( && ) : ('n, bool) expr -> ('n, bool) expr -> ('n, bool) expr
  val ( &&? ) : ('n, bool) expr -> ('n, bool) expr option -> ('n, bool) expr
  val ( || ) : ('n, bool) expr -> ('n, bool) expr -> ('n, bool) expr
  val ( ||? ) : ('n, bool) expr -> ('n, bool) expr option -> ('n, bool) expr
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
  val coalesce : (_, 'a) expr list -> else_:('n, 'a) expr -> ('n, 'a) expr
  val toNullable : (_, 'a) expr -> (null nullable, 'a) expr

  val nullIf : ('n, 'a) expr -> ('n, 'a) expr -> (null, 'a) expr
  (** [nullIf x y] returns NULL if [x] equals [y], otherwise returns [x]. *)

  val ifNull : (null, 'a) expr -> ('n, 'a) expr -> ('n, 'a) expr
  (** [ifNull x alt] returns [alt] if [x] is NULL, otherwise returns [x]. *)

  (** {2 String} *)

  val empty : ('n, string) expr -> ('n, bool) expr
  val notEmpty : ('n, string) expr -> ('n, bool) expr
  val lowerUTF8 : ('n, string) expr -> ('n, string) expr

  val concat : ('n, string) expr list -> ('n, string) expr
  (** Concatenates strings. *)

  val substring :
    ('n, string) expr ->
    (non_null, int number) expr ->
    (non_null, int number) expr ->
    ('n, string) expr
  (** [substring s offset length] extracts a substring starting at [offset]
      (1-based) with [length] bytes. *)

  val substringUTF8 :
    ('n, string) expr ->
    (non_null, int number) expr ->
    (non_null, int number) expr ->
    ('n, string) expr
  (** [substringUTF8 s offset length] extracts a substring starting at [offset]
      (1-based) with [length] Unicode code points. *)

  val match_ : ('n, string) expr -> (non_null, string) expr -> ('n, bool) expr

  (** {2 String replacement} *)

  val replaceOne :
    ('n, string) expr ->
    (non_null, string) expr ->
    (non_null, string) expr ->
    ('n, string) expr

  (** {2 String search} *)

  val like : ('n, string) expr -> (non_null, string) expr -> ('n, bool) expr
  val endsWith : ('n, string) expr -> (non_null, string) expr -> ('n, bool) expr

  val startsWith :
    ('n, string) expr -> (non_null, string) expr -> ('n, bool) expr

  val position :
    ('n, string) expr -> (non_null, string) expr -> ('n, int number) expr
  (** Returns 1-indexed position of first occurrence of needle in haystack, 0 if
      not found. *)

  val positionCaseInsensitive :
    ('n, string) expr -> (non_null, string) expr -> ('n, int number) expr
  (** Case-insensitive version of [position]. *)

  val positionUTF8 :
    ('n, string) expr -> (non_null, string) expr -> ('n, int number) expr
  (** UTF-8 aware version of [position]. Returns position in Unicode code
      points. *)

  val positionCaseInsensitiveUTF8 :
    ('n, string) expr -> (non_null, string) expr -> ('n, int number) expr
  (** Case-insensitive UTF-8 aware version of [position]. *)

  val locate :
    ('n, string) expr -> (non_null, string) expr -> ('n, int number) expr
  (** Alias for [position]. *)

  val multiSearchFirstPosition :
    ('n, string) expr ->
    (non_null, (non_null, string) array) expr ->
    ('n, int number) expr
  (** Returns the leftmost offset in a haystack string which matches any of
      multiple needle strings, otherwise 0, if there was no match *)

  val multiMatchAllIndices :
    ('n, string) expr ->
    (non_null, (non_null, string) array) expr ->
    (non_null, (non_null, uint64 number) array) expr
  (** Returns an array of indices of all matching patterns (1-based). *)

  val extract :
    ('n, string) expr -> (non_null, string) expr -> ('n, string) expr
  (** [extract haystack pattern] extracts the first regex match. Returns empty
      string if no match found. Uses RE2 regex library. *)

  (** {2 URL functions} *)

  val extractURLParameter :
    ('n, string) expr -> (non_null, string) expr -> ('n, string) expr
  (** Returns the value of the 'name' parameter in the URL, if present,
      otherwise an empty string. *)

  val decodeURLComponent : ('n, string) expr -> ('n, string) expr
  (** Decodes a URL-encoded string. *)

  (** {2 JSON functions} *)

  val simpleJSONExtractString :
    ('n, string) expr -> (non_null, string) expr -> ('n, string) expr
  (** [simpleJSONExtractString json field] extracts a string value from JSON.
      Returns empty string if field doesn't exist or isn't a string. *)

  val simpleJSONExtractInt :
    ('n, string) expr -> (non_null, string) expr -> ('n, int64 number) expr
  (** [simpleJSONExtractInt json field] extracts an integer value from JSON.
      Returns 0 if field doesn't exist or isn't numeric. *)

  val simpleJSONExtractFloat :
    ('n, string) expr -> (non_null, string) expr -> ('n, float number) expr
  (** [simpleJSONExtractFloat json field] extracts a float value from JSON.
      Returns 0 if field doesn't exist or isn't numeric. *)

  val simpleJSONExtractBool :
    ('n, string) expr -> (non_null, string) expr -> ('n, bool) expr
  (** [simpleJSONExtractBool json field] extracts a boolean value from JSON.
      Returns false if field doesn't exist or isn't a boolean. *)

  val simpleJSONExtractRaw :
    ('n, string) expr -> (non_null, string) expr -> ('n, string) expr
  (** [simpleJSONExtractRaw json field] extracts raw unparsed value from JSON.
      Returns empty string if field doesn't exist. *)

  val jsonExtractKeys :
    ('n, string) expr -> (non_null, (non_null, string) array) expr
  (** [jsonExtractKeys json] extracts keys from a JSON object. Returns array of
      key names. *)

  (** {2 Splitting functions} *)

  val splitByChar :
    (non_null, string) expr ->
    ('n, string) expr ->
    ('n, (non_null, string) array) expr
  (** [splitByChar separator s] splits a string by a single character separator.
      Returns array of substrings. *)

  (** {2 Type conversions} *)

  val toInt32 : ('n, _) expr -> ('n, int number) expr
  val toInt64 : ('n, _) expr -> ('n, int64 number) expr
  val toUInt64 : ('n, _) expr -> ('n, uint64 number) expr
  val toUInt32 : ('n, _) expr -> ('n, int number) expr

  val toUInt32OrDefault :
    ('n, _) expr -> (non_null, int number) expr -> (non_null, int number) expr

  val toFloat32 : ('n, _) expr -> ('n, float number) expr
  val toFloat64 : ('n, _) expr -> ('n, float number) expr
  val toString : ('n, _) expr -> ('n, string) expr

  val isFinite : ('n, float number) expr -> ('n, bool) expr
  (** Returns 1 if the Float32/Float64 argument is not infinite and not NaN,
      otherwise returns 0. *)

  val isInfinite : ('n, float number) expr -> ('n, bool) expr
  (** Returns 1 if the Float32/Float64 argument is infinite, otherwise returns 0.
      Note that 0 is returned for NaN. *)

  val isNaN : ('n, float number) expr -> ('n, bool) expr
  (** Returns 1 if the Float32/Float64 argument is NaN, otherwise returns 0. *)

  val ifNotFinite :
    ('n, float number) expr -> ('n, float number) expr -> ('n, float number) expr
  (** [ifNotFinite x y] returns [x] if [x] is finite, otherwise returns [y]. *)

  val isNull : ('n, _) expr -> (non_null, bool) expr
  (** Returns 1 if the argument is NULL, 0 otherwise. *)

  val isNotNull : ('n, _) expr -> (non_null, bool) expr
  (** Returns 1 if the argument is not NULL, 0 otherwise. *)

  (** {2 Conditional} *)

  val greatest : ('n, 'a) expr list -> ('n, 'a) expr
  (** Returns the greatest value from the arguments. *)

  val least : ('n, 'a) expr list -> ('n, 'a) expr
  (** Returns the least value from the arguments. *)

  val avg2 : ('n, 'a) expr -> ('n, 'a) expr -> ('n, 'a) expr
  (** Returns the average value of two arguments. *)

  (** {2 Hash functions} *)

  val farmFingerprint64 : ('n, _) expr -> ('n, uint64 number) expr
  (** Produces a 64-bit FarmHash fingerprint. *)

  (** {2 Rounding functions} *)

  val round : ('n, 'a number) expr -> ('n, 'a number) expr
  (** Rounds a value to the nearest integer. *)

  (** {2 Bit functions} *)

  val byteSwap : ('n, 'a number) expr -> ('n, 'a number) expr
  (** Reverses the bytes of an integer, i.e. changes its endianness. *)

  (** {2 Machine learning functions} *)

  val arrayAUCPR :
    ?partial_offsets:('n, ('m, 'b number) array) expr ->
    ('n, ('m, 'a number) array) expr ->
    ('n, ('m, 'c number) array) expr ->
    ('n, float number) expr
  (** [arrayAUCPR ?partial_offsets scores labels] calculates the area under the
      precision-recall (PR) curve. Returns a value between 0 and 1. *)

  (** {1 Aggregate functions} *)

  val avg :
    ?partition_by:a_expr list ->
    ?order_by:(a_expr * [ `ASC | `DESC ]) list ->
    ('n, _ number) expr ->
    (non_null, float number) expr

  val count :
    ?partition_by:a_expr list ->
    ?order_by:(a_expr * [ `ASC | `DESC ]) list ->
    ('n, _) expr ->
    (non_null, int64 number) expr

  val sum :
    ?partition_by:a_expr list ->
    ?order_by:(a_expr * [ `ASC | `DESC ]) list ->
    ('n, 't number) expr ->
    (non_null, 't number) expr

  val uniq :
    ?partition_by:a_expr list ->
    ?order_by:(a_expr * [ `ASC | `DESC ]) list ->
    ('n, _) expr ->
    (non_null, int64 number) expr

  val uniqExact :
    ?partition_by:a_expr list ->
    ?order_by:(a_expr * [ `ASC | `DESC ]) list ->
    ('n, _) expr ->
    (non_null, int64 number) expr

  val min :
    ?partition_by:a_expr list ->
    ?order_by:(a_expr * [ `ASC | `DESC ]) list ->
    ('n, 'a) expr ->
    ('n, 'a) expr

  val max :
    ?partition_by:a_expr list ->
    ?order_by:(a_expr * [ `ASC | `DESC ]) list ->
    ('n, 'a) expr ->
    ('n, 'a) expr

  val any : ('n, 'a) expr -> ('n, 'a) expr
  (** Selects the first encountered value. *)

  val anyLast : ('n, 'a) expr -> ('n, 'a) expr
  (** Selects the last encountered value. *)

  val argMin : ('n, 'a) expr -> (_, _) expr -> ('n, 'a) expr
  (** [argMin arg val] returns [arg] value at the row with minimum [val]. *)

  val argMax : ('n, 'a) expr -> (_, _) expr -> ('n, 'a) expr
  (** [argMax arg val] returns [arg] value at the row with maximum [val]. *)

  val groupArray : ('n, 'a) expr -> (non_null, ('n, 'a) array) expr
  (** Creates an array of argument values. *)

  val groupUniqArray : ('n, 'a) expr -> (non_null, ('n, 'a) array) expr
  (** Creates an array from different argument values (unique). *)

  (** {2 Aggregate functions with -If suffix} *)

  val avgIf :
    ('n, _ number) expr -> (_, bool) expr -> (non_null, float number) expr

  val countIf : ('n, _) expr -> (_, bool) expr -> (non_null, int64 number) expr

  val sumIf :
    ('n, 't number) expr -> (_, bool) expr -> (non_null, 't number) expr

  val uniqIf : ('n, _) expr -> (_, bool) expr -> (non_null, int64 number) expr
  val minIf : ('n, 'a) expr -> (_, bool) expr -> ('n, 'a) expr
  val maxIf : ('n, 'a) expr -> (_, bool) expr -> ('n, 'a) expr
  val anyIf : ('n, 'a) expr -> (_, bool) expr -> ('n, 'a) expr
  val anyLastIf : ('n, 'a) expr -> (_, bool) expr -> ('n, 'a) expr
  val argMinIf : ('n, 'a) expr -> (_, _) expr -> (_, bool) expr -> ('n, 'a) expr
  val argMaxIf : ('n, 'a) expr -> (_, _) expr -> (_, bool) expr -> ('n, 'a) expr

  val groupArrayIf :
    ('n, 'a) expr -> (_, bool) expr -> (non_null, ('n, 'a) array) expr

  val groupUniqArrayIf :
    ('n, 'a) expr -> (_, bool) expr -> (non_null, ('n, 'a) array) expr

  (** {2 Aggregate functions with -State suffix} *)

  val avgState :
    ('n, _ number) expr -> (non_null, (non_null, float number) agg_state) expr

  val countState :
    ('n, _) expr -> (non_null, (non_null, int64 number) agg_state) expr

  val sumState :
    ('n, 't number) expr -> (non_null, (non_null, 't number) agg_state) expr

  val uniqState :
    ('n, _) expr -> (non_null, (non_null, int64 number) agg_state) expr

  val uniqExactState :
    ('n, _) expr -> (non_null, (non_null, int64 number) agg_state) expr

  val minState : ('n, 'a) expr -> (non_null, ('n, 'a) agg_state) expr
  val maxState : ('n, 'a) expr -> (non_null, ('n, 'a) agg_state) expr
  val anyState : ('n, 'a) expr -> (non_null, ('n, 'a) agg_state) expr
  val anyLastState : ('n, 'a) expr -> (non_null, ('n, 'a) agg_state) expr

  val argMinState :
    ('n, 'a) expr -> (_, _) expr -> (non_null, ('n, 'a) agg_state) expr

  val argMaxState :
    ('n, 'a) expr -> (_, _) expr -> (non_null, ('n, 'a) agg_state) expr

  val groupArrayState :
    ('n, 'a) expr -> (non_null, (non_null, ('n, 'a) array) agg_state) expr

  val groupUniqArrayState :
    ('n, 'a) expr -> (non_null, (non_null, ('n, 'a) array) agg_state) expr

  (** {2 Aggregate functions with -StateIf suffix} *)

  val avgStateIf :
    ('n, _ number) expr ->
    (_, bool) expr ->
    (non_null, (non_null, float number) agg_state) expr

  val countStateIf :
    ('n, _) expr ->
    (_, bool) expr ->
    (non_null, (non_null, int64 number) agg_state) expr

  val sumStateIf :
    ('n, 't number) expr ->
    (_, bool) expr ->
    (non_null, (non_null, 't number) agg_state) expr

  val uniqStateIf :
    ('n, _) expr ->
    (_, bool) expr ->
    (non_null, (non_null, int64 number) agg_state) expr

  val minStateIf :
    ('n, 'a) expr -> (_, bool) expr -> (non_null, ('n, 'a) agg_state) expr

  val maxStateIf :
    ('n, 'a) expr -> (_, bool) expr -> (non_null, ('n, 'a) agg_state) expr

  val anyStateIf :
    ('n, 'a) expr -> (_, bool) expr -> (non_null, ('n, 'a) agg_state) expr

  val anyLastStateIf :
    ('n, 'a) expr -> (_, bool) expr -> (non_null, ('n, 'a) agg_state) expr

  val argMinStateIf :
    ('n, 'a) expr ->
    (_, _) expr ->
    (_, bool) expr ->
    (non_null, ('n, 'a) agg_state) expr

  val argMaxStateIf :
    ('n, 'a) expr ->
    (_, _) expr ->
    (_, bool) expr ->
    (non_null, ('n, 'a) agg_state) expr

  val groupArrayStateIf :
    ('n, 'a) expr ->
    (_, bool) expr ->
    (non_null, (non_null, ('n, 'a) array) agg_state) expr

  val groupUniqArrayStateIf :
    ('n, 'a) expr ->
    (_, bool) expr ->
    (non_null, (non_null, ('n, 'a) array) agg_state) expr

  (** {2 Aggregate functions with -Merge suffix} *)

  val avgMerge :
    (_, (non_null, float number) agg_state) expr ->
    (non_null, float number) expr

  val countMerge :
    (_, (non_null, int64 number) agg_state) expr ->
    (non_null, int64 number) expr

  val sumMerge :
    (_, (non_null, 't number) agg_state) expr -> (non_null, 't number) expr

  val uniqMerge :
    (_, (non_null, int64 number) agg_state) expr ->
    (non_null, int64 number) expr

  val uniqExactMerge :
    (_, (non_null, int64 number) agg_state) expr ->
    (non_null, int64 number) expr

  val minMerge : (_, ('n, 'a) agg_state) expr -> ('n, 'a) expr
  val maxMerge : (_, ('n, 'a) agg_state) expr -> ('n, 'a) expr
  val anyMerge : (_, ('n, 'a) agg_state) expr -> ('n, 'a) expr
  val anyLastMerge : (_, ('n, 'a) agg_state) expr -> ('n, 'a) expr
  val argMinMerge : (_, ('n, 'a) agg_state) expr -> ('n, 'a) expr
  val argMaxMerge : (_, ('n, 'a) agg_state) expr -> ('n, 'a) expr

  val groupArrayMerge :
    (_, (non_null, ('n, 'a) array) agg_state) expr ->
    (non_null, ('n, 'a) array) expr

  val groupUniqArrayMerge :
    (_, (non_null, ('n, 'a) array) agg_state) expr ->
    (non_null, ('n, 'a) array) expr

  (** {2 Aggregate combinators} *)

  val finalizeAggregation : (_, ('n, 'a) agg_state) expr -> ('n, 'a) expr
  (** Converts an aggregate function state to its final result. *)

  val sumForEach :
    ('n, (non_null, 't number) array) expr ->
    (non_null, (non_null, 't number) array) expr
  (** Applies sum to each element across arrays. *)

  val sumMap :
    ('n, 'k) expr ->
    ('n, 'v number) expr ->
    (non_null, ('n, 'k, non_null, 'v number) map) expr
  (** Sums values for each key across rows. *)

  val sumMapState :
    ('n, 'k) expr ->
    ('n, 'v number) expr ->
    (non_null, (non_null, ('n, 'k, non_null, 'v number) map) agg_state) expr
  (** sumMap with State combinator. *)

  val sumMapMerge :
    (_, (non_null, ('nk, 'k, 'nv, 'v number) map) agg_state) expr ->
    (non_null, ('nk, 'k, 'nv, 'v number) map) expr
  (** sumMap with Merge combinator. *)

  val sumMapMergeState :
    ('n, 'k) expr ->
    ('n, 'v number) expr ->
    (non_null, (non_null, ('n, 'k, non_null, 'v number) map) agg_state) expr
  (** sumMap with Merge and State combinators. *)

  val avgMergeStateIf :
    (_, (non_null, float number) agg_state) expr ->
    (_, bool) expr ->
    (non_null, (non_null, float number) agg_state) expr
  (** avgMerge with State and If combinators. *)

  (* {2 Join/Dict table functions} *)

  val joinGet :
    ((non_null, 'keys) expr, 'values) Dict.t ->
    ('n, 'a) expr ->
    (non_null, 'keys) expr ->
    ('n, 'a) expr

  val joinGetOrNull :
    ((non_null, 'keys) expr, 'values) Dict.t ->
    ('n, 'a) expr ->
    (non_null, 'keys) expr ->
    (null, 'a) expr

  val dictGet :
    ((non_null, 'key) expr, 'values) Dict.t ->
    ('n, 'a) expr ->
    (non_null, 'key) expr ->
    ('n, 'a) expr

  (* {2 Tuples} *)

  val tuple2 :
    ('na, 'a) expr * ('nb, 'b) expr ->
    (non_null, (('na, 'a) expr, ('nb, 'b) expr) tuple2) expr

  val tuple3 :
    ('na, 'a) expr * ('nb, 'b) expr * ('nc, 'c) expr ->
    (non_null, (('na, 'a) expr, ('nb, 'b) expr, ('nc, 'c) expr) tuple3) expr

  val tuple4 :
    ('na, 'a) expr * ('nb, 'b) expr * ('nc, 'c) expr * ('nd, 'd) expr ->
    ( non_null,
      (('na, 'a) expr, ('nb, 'b) expr, ('nc, 'c) expr, ('nd, 'd) expr) tuple4
    )
    expr
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

module Parse : sig
  type ('null, 'sql_type, 'ocaml_type) t

  val string : (non_null, string, string) t
  val bool : (non_null, bool, bool) t
  val int : (non_null, int number, int) t
  val int64 : (non_null, int64 number, int64) t
  val uint64 : (non_null, uint64 number, uint64) t
  val float : (non_null, float number, float) t
  val date : (non_null, date, float) t
  val datetime : (non_null, datetime, float) t

  val any : (_, _, json) t
  (** [any] parses any JSON value as-is. Returns JSON. *)

  val custom : (json -> 'a) * ('a -> ('n, 's) expr) -> ('n, 's, 'a) t
  (** [custom f] parses a JSON value with [f] function. *)

  val nullable : ('n, 's, 'o) t -> (null, 's, 'o option) t
  (** [nullable p] parses a nullable value with parser [p]. If the JSON value is
      [Null], returns [None], otherwise parses with [p] and returns [Some v]. *)

  val array : ('n, 's, 'o) t -> (non_null, ('n, 's) array, 'o list) t
  (** [array p] parses an array of values with parser [p]. *)

  val map :
    ('nk, 'sk, 'ok) t ->
    ('nv, 'sv, 'ov) t ->
    (non_null, ('nk, 'sk, 'nv, 'sv) map, ('ok * 'ov) list) t
  (** [map pk pv] parses a map with key parser [pk] and value parser [pv]. The
      result is a list of key-value pairs. *)

  exception Parse_error of json option * string

  val parse_error : ?json:json -> string -> 'a
  (** A convenience function to raise [Parse_error]. *)

  val parse : (_, _, 'a) t -> json -> 'a
  (** Parse a value from JSON.

      Raises [Parse_error] if the value is not compatible with the parser. *)

  val unparse : ('n, 's, 'a) t -> 'a -> ('n, 's) expr
  (** Unparse an OCaml value into SQL expression. *)
end

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

  val col : ('n, 's) expr -> ('n, 's, 'o) Parse.t -> 'o t
  (** Query a single column. *)

  val row : a_expr list -> (json list -> 'a) -> 'a t
  (** Query multiple columns.

      [Row.row exprs parse] queries columns represented by [exprs] and parses
      them with [parse] function. The [parse] function receives a JSON list
      representing the row. *)

  val ignore : ('n, 'a) expr -> unit t
  (** Query a single column but ignore its value. *)

  val parse : 'a t -> json list -> 'a
  (** Parse a row from a JSON list.

      Raises [Parse.Parse_error] if the row is not compatible with the parser.
  *)

  val exprs : _ t -> a_expr list
  (** Get the list of expressions representing the row. *)
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
