open Ppx_hash_lib.Std.Hash.Builtin
open Ppx_compare_lib.Builtin

type 'a node = { node : 'a; loc : Loc.t; eq : 'a Eq_class.t }

let hash_fold_node _ h { node = _; loc = _; eq } = Eq_class.hash_fold_t h eq
let equal_node _ a b = Eq_class.equal a.eq b.eq
let compare_node _ a b = Eq_class.compare a.eq b.eq

type id = string node [@@deriving hash, equal, compare]

type expr = exprsyn node [@@deriving hash, equal]

and exprsyn =
  | E_col of id * id
  | E_query of id * expr
  | E_id of id
  | E_lit of lit
  | E_call of func * expr list
  | E_window of id * expr list * window_spec
  | E_param of param
  | E_ocaml_expr of string
  | E_in of expr * in_query
  | E_lambda of id list * expr
  | E_unsafe of string node
  | E_unsafe_concat of expr list
  | E_ascribe of expr * typ

and param = { param : id; param_has_scope : bool; param_optional : bool }
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

and interpolate_item = { interpolate_col : id; interpolate_expr : expr option }

and with_fill = {
  fill_from : expr option;
  fill_to : expr option;
  fill_step : expr option;
  fill_interpolate : interpolate_item list;
}

and order_by =
  | Order_by_expr of expr * [ `ASC | `DESC ] * with_fill option
  | Order_by_splice of param

(** used for GROUP BY *)
and dimension = Dimension_expr of expr | Dimension_splice of param

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
  | Q_param of param
  | Q_ascribe of query * typ

and setting = Setting_lit of lit | Setting_param of param
and setting_item = Setting_item of id * setting | Setting_splice of param
and select = Select_fields of field list | Select_splice of param
and from_one = from_onesyn node
and from = fromsyn node

and from_onesyn =
  | F_table of { db : id; table : id; alias : id; final : bool }
  | F_select of {
      select : query;
      alias : id;
      cluster_name : cluster_name option;
    }
  | F_param of { param : param; alias : id; final : bool }
  | F_ascribe of from_one * typ
  | F_call of { db : id; table : id; args : (id * expr) list; alias : id }

and cluster_name = Cluster_name of id | Cluster_name_param of param

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

module Eq_expr = Eq_class.Make (struct
  type t = exprsyn

  let hash = hash_exprsyn
  let equal = equal_exprsyn
end)

module Eq_typ = Eq_class.Make (struct
  type t = typsyn

  let hash = hash_typsyn
  let equal = equal_typsyn
end)

module Eq_query = Eq_class.Make (struct
  type t = querysyn

  let hash = hash_querysyn
  let equal = equal_querysyn
end)

module Eq_id = Eq_class.Make (struct
  type t = string

  let hash = hash_string
  let equal = equal_string
end)

module Eq_from_one = Eq_class.Make (struct
  type t = from_onesyn

  let hash = hash_from_onesyn
  let equal = equal_from_onesyn
end)

module Eq_from = Eq_class.Make (struct
  type t = fromsyn

  let hash = hash_fromsyn
  let equal = equal_fromsyn
end)

let make_typ ?(loc = Loc.dummy) node = { node; loc; eq = Eq_typ.v node }
let make_expr ?(loc = Loc.dummy) node = { node; loc; eq = Eq_expr.v node }
let make_query ?(loc = Loc.dummy) node = { node; loc; eq = Eq_query.v node }
let make_id ?(loc = Loc.dummy) node = { node; loc; eq = Eq_id.v node }

let make_from_one ?(loc = Loc.dummy) node =
  { node; loc; eq = Eq_from_one.v node }

let make_from ?(loc = Loc.dummy) node = { node; loc; eq = Eq_from.v node }

let is_scope_typ typ =
  match typ.node with
  | T_scope _ | T_db_table _ -> true
  | T _ | T_app _ | T_any | T_custom _ -> false
