type id = string Loc.with_loc
type order_direction = ASC | DESC

type expr = exprsyn Loc.with_loc

and exprsyn =
  | E_col of id * id
  | E_lit of lit
  | E_call of func * expr list
      (** encodes function calls and operators as well *)
  | E_window of id * expr list * window_spec
      (** window function with OVER clause *)
  | E_value of id  (** variable_name for splicing OCaml values *)
  | E_ocaml_expr of string  (** OCaml expression for splicing *)
  | E_in of expr * in_query

and func = Func of id | Func_method of id * id
and lit = L_int of int | L_bool of bool | L_string of string
and in_query = In_query of query | In_query_param of id

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

and querysyn = {
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
