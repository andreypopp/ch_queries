type 'a node = private { node : 'a; loc : Loc.t; eq : 'a Eq_class.t }
[@@deriving hash, equal]
(** Data annotated with its location in the source code along with an
    equivalence class (used for fast equality comparison and fast hashing). *)

type id = string node [@@deriving hash, equal, compare]

type expr = exprsyn node [@@deriving hash, equal]

and exprsyn =
  | E_col of id * id  (** column reference, e.g. `table.col` *)
  | E_query of id * expr
      (** an expression built within another scope, e.g. `table.(age + 1)` *)
  | E_id of id
  | E_lit of lit  (** literal value, e.g. `42`, `true`, `'hello'` *)
  | E_call of func * expr list
      (** encodes function calls and operators as well *)
  | E_window of id * expr list * window_spec
      (** window function, e.g. f(..)OVER(...) *)
  | E_param of id  (** param for splicing OCaml values, ?param *)
  | E_ocaml_expr of string  (** OCaml expression for splicing *)
  | E_in of expr * in_query
      (** in-query, e.g. `expr IN (query)` or `expr IN (expr)` *)
  | E_lambda of id * expr  (** lambda expression: param -> body *)
  | E_unsafe of string node  (** unsafe injection of an SQL fragment *)
  | E_unsafe_concat of expr list  (** unsafe concatenation of SQL fragments *)
  | E_ascribe of expr * typ  (** type ascription, e.g. `expr: Typ` *)

and func = Func of id | Func_method of id * id

and lit =
  | L_int of int
  | L_bool of bool
  | L_string of string
  | L_float of float
  | L_null
  | L_interval of int * interval_unit

and interval_unit = Year | Month | Week | Day | Hour | Minute | Second
and in_query = In_query of query | In_expr of expr

and window_spec = {
  partition_by : dimension list option;
  order_by : order_by list option;
}

and order_by =
  | Order_by_expr of expr * [ `ASC | `DESC ]
  | Order_by_splice of id

(** used for GROUP BY *)
and dimension = Dimension_expr of expr | Dimension_splice of id

and field = { expr : expr; alias : id option }
(** used for SELECT *)

and with_field =
  | With_expr of field  (** WITH <expr> AS <id> *)
  | With_query of id * query * bool  (** WITH <id> AS <query> *)

and query = querysyn node

and querysyn =
  | Q_select of {
      with_fields : with_field list;
      select : select;
      from : from;
      prewhere : expr option;
      where : expr option;
      qualify : expr option;
      group_by : dimension list option;
      having : expr option;
      order_by : order_by list option;
      limit : expr option;
      offset : expr option;
      settings : setting_item list;
    }
  | Q_union of query * query
  | Q_param of id
  | Q_ascribe of query * typ

and setting = Setting_lit of lit | Setting_param of id
and setting_item = Setting_item of id * setting | Setting_splice of id
and select = Select_fields of field list | Select_splice of id
and from_one = from_onesyn node
and from = fromsyn node

and from_onesyn =
  | F_table of { db : id; table : id; alias : id; final : bool }
  | F_select of {
      select : query;
      alias : id;
      cluster_name : cluster_name option;
    }
  | F_param of { id : id; alias : id; final : bool }
  | F_ascribe of from_one * typ

and cluster_name = Cluster_name of id | Cluster_name_param of id

and fromsyn =
  | F of from_one
  | F_join of {
      kind : [ `INNER_JOIN | `LEFT_JOIN | `LEFT_JOIN_OPTIONAL ];
      from : from;
      join : from_one;
      on : expr;
    }

and typ = typsyn node

and typsyn =
  | T of id
  | T_app of id * typ list
  | T_any
  | T_db_table of id * id * [ `NON_NULL | `NULL ] (* database, table *)
  | T_scope of scope_column list * [ `Open | `Closed ] * [ `NON_NULL | `NULL ]
  | T_custom of id (* Custom(<ocamltype>) *)

and scope_column = { name : id; typ : typ }

val make_id : ?loc:Loc.t -> string -> id
val make_expr : ?loc:Loc.t -> exprsyn -> expr
val make_query : ?loc:Loc.t -> querysyn -> query
val make_from : ?loc:Loc.t -> fromsyn -> from
val make_from_one : ?loc:Loc.t -> from_onesyn -> from_one
val make_typ : ?loc:Loc.t -> typsyn -> typ

val is_scope_typ : typ -> bool
(** Returns true if the type is a scope type. *)
