type id = string Loc.with_loc

type expr = exprsyn Loc.with_loc

and exprsyn =
  | E_col of id * id  (** column reference, e.g. `table.col` *)
  | E_lit of lit  (** literal value, e.g. `42`, `true`, `'hello'` *)
  | E_call of func * expr list
      (** encodes function calls and operators as well *)
  | E_window of id * expr list * window_spec
      (** window function, e.g. f(..)OVER(...) *)
  | E_param of id * typ option
      (** param for splicing OCaml values, ?param or ?param:typ *)
  | E_ocaml_expr of string  (** OCaml expression for splicing *)
  | E_in of expr * in_query
      (** in-query, e.g. `expr IN (query)` or `expr IN (expr)` *)
  | E_lambda of id * expr  (** lambda expression: param -> body *)
  | E_unsafe of string Loc.with_loc  (** unsafe injection of an SQL fragment *)
  | E_unsafe_concat of expr list  (** unsafe concatenation of SQL fragments *)

and func = Func of id | Func_method of id * id
and lit = L_int of int | L_bool of bool | L_string of string
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

and query = querysyn Loc.with_loc

and querysyn =
  | Q_select of {
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
    }
  | Q_union of query * query
  | Q_param of id

and select = Select_fields of field list | Select_splice of id
and from_one = from_onesyn Loc.with_loc
and from = fromsyn Loc.with_loc

and from_onesyn =
  | F_table : { db : id; table : id; alias : id; final : bool } -> from_onesyn
  | F_select : {
      select : query;
      alias : id;
      cluster_name : cluster_name option;
    }
      -> from_onesyn
  | F_value : { id : id; alias : id } -> from_onesyn

and cluster_name = Cluster_name of id | Cluster_name_param of id

and fromsyn =
  | F : from_one -> fromsyn
  | F_join : {
      kind : [ `INNER_JOIN | `LEFT_JOIN ];
      from : from;
      join : from_one;
      on : expr;
    }
      -> fromsyn

and typ = typsyn Loc.with_loc
and typsyn = T of id | T_app of id * typ list
