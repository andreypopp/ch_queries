type id = string Loc.with_loc

type expr = exprsyn Loc.with_loc

and exprsyn =
  | E_col of id * id
  | E_lit of lit
  | E_call of id * expr list
      (** encodes function calls and operators as well *)
  | E_window of id * expr list * window_spec
      (** window function with OVER clause *)
  | E_value of id  (** variable_name for splicing OCaml values *)

and lit = L_int of int | L_bool of bool | L_string of string

and window_spec = {
  partition_by : dimension list option;
  order_by : order_by list option;
}

and order_by =
  | Order_by_expr of expr * [ `ASC | `DESC ]
  | Order_by_splice of id

(** used for GROUP BY *)
and dimension = Dimension_expr of expr | Dimension_splice of id

type field = { expr : expr; alias : id option }
(** used for SELECT *)

type order_direction = ASC | DESC

type query = querysyn Loc.with_loc

and querysyn = {
  fields : field list;
  from : from;
  where : expr option;
  qualify : expr option;
  group_by : dimension list option;
  having : expr option;
  order_by : order_by list option;
  limit : expr option;
  offset : expr option;
}

and from_one = from_onesyn Loc.with_loc
and from = fromsyn Loc.with_loc

and from_onesyn =
  | F_table : { db : id; table : id; alias : id } -> from_onesyn
  | F_select : { select : query; alias : id } -> from_onesyn
  | F_value : { id : id; alias : id } -> from_onesyn

and fromsyn =
  | F : from_one -> fromsyn
  | F_join : {
      kind : [ `INNER_JOIN | `LEFT_JOIN ];
      from : from;
      join : from_one;
      on : expr;
    }
      -> fromsyn
