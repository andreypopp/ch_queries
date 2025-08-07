open PPrint
open Syntax

let pp_id id = string id.node

let escape_single_quoted s =
  let buf = Buffer.create (String.length s) in
  String.iter s ~f:(function
    | '\'' -> Buffer.add_string buf "\\'"
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\t' -> Buffer.add_string buf "\\t"
    | '\r' -> Buffer.add_string buf "\\r"
    | c -> Buffer.add_char buf c);
  Buffer.contents buf

let rec pp_expr expr =
  match expr.node with
  | E_unsafe_concat xs -> group (separate_map empty pp_expr xs)
  | E_unsafe id -> string id.node
  | E_col (ns, id) -> string (Printf.sprintf "%s.%s" ns.node id.node)
  | E_lit (L_int n) -> string (string_of_int n)
  | E_lit (L_float n) -> string (string_of_float n)
  | E_lit L_null -> string "NULL"
  | E_lit (L_bool b) -> string (string_of_bool b)
  | E_lit (L_string s) ->
      string (Printf.sprintf "'%s'" (escape_single_quoted s))
  | E_param (v, typ) -> (
      let base = string v.node in
      match typ with None -> base | Some t -> base ^^ string ":" ^^ pp_typ t)
  | E_window (name, args, window_spec) ->
      let pp_args =
        match args with
        | [] -> empty
        | _ -> separate (string "," ^^ space) (List.map ~f:pp_expr args)
      in
      let pp_partition_by =
        match window_spec.partition_by with
        | None -> empty
        | Some dimensions ->
            string "PARTITION BY" ^^ space
            ^^ separate
                 (string "," ^^ space)
                 (List.map ~f:pp_dimension dimensions)
      in
      let pp_order_by =
        match window_spec.order_by with
        | None -> empty
        | Some orders ->
            let pp_order = function
              | Order_by_expr (expr, `ASC) ->
                  pp_expr expr ^^ space ^^ string "ASC"
              | Order_by_expr (expr, `DESC) ->
                  pp_expr expr ^^ space ^^ string "DESC"
              | Order_by_splice id -> pp_id id
            in
            string "ORDER BY" ^^ space
            ^^ separate (string "," ^^ space) (List.map ~f:pp_order orders)
      in
      let pp_over_clause =
        let content =
          match (window_spec.partition_by, window_spec.order_by) with
          | None, None -> empty
          | Some _, None -> pp_partition_by
          | None, Some _ -> pp_order_by
          | Some _, Some _ -> pp_partition_by ^^ space ^^ pp_order_by
        in
        string "OVER" ^^ space ^^ string "(" ^^ content ^^ string ")"
      in
      group
        (pp_id name ^^ string "(" ^^ pp_args ^^ string ")" ^^ space
       ^^ pp_over_clause)
  | E_call (func, args) -> (
      match func with
      | Func name -> (
          match (name.node, args) with
          | "+", [ left; right ] ->
              group (pp_expr left ^/^ string "+" ^/^ pp_expr right)
          | "[", xs ->
              brackets (List.map ~f:pp_expr xs |> separate (string "," ^^ space))
          | "-", [ left; right ] ->
              group (pp_expr left ^/^ string "-" ^/^ pp_expr right)
          | "*", [ left; right ] ->
              group (pp_expr left ^/^ string "*" ^/^ pp_expr right)
          | "/", [ left; right ] ->
              group (pp_expr left ^/^ string "/" ^/^ pp_expr right)
          | "AND", [ left; right ] ->
              group (pp_expr left ^/^ string "AND" ^/^ pp_expr right)
          | "OR", [ left; right ] ->
              group (pp_expr left ^/^ string "OR" ^/^ pp_expr right)
          | "=", [ left; right ] ->
              group (pp_expr left ^/^ string "=" ^/^ pp_expr right)
          | _, _ ->
              let pp_args =
                match args with
                | [] -> empty
                | _ -> separate (string "," ^^ space) (List.map ~f:pp_expr args)
              in
              group (pp_id name ^^ string "(" ^^ pp_args ^^ string ")"))
      | Func_method (table, method_name) ->
          let pp_args =
            match args with
            | [] -> empty
            | _ -> separate (string "," ^^ space) (List.map ~f:pp_expr args)
          in
          group
            (pp_id table ^^ string "." ^^ pp_id method_name ^^ string "("
           ^^ pp_args ^^ string ")"))
  | E_ocaml_expr s -> string ("?{" ^ s ^ "}")
  | E_in (expr, in_query) -> (
      match in_query with
      | In_query query ->
          group
            (pp_expr expr ^/^ string "IN" ^/^ lparen
            ^^ nest 2 (break 0 ^^ pp_query query ^^ rparen))
      | In_expr expr' -> group (pp_expr expr ^/^ string "IN" ^/^ pp_expr expr'))
  | E_lambda (param, body) ->
      group (pp_id param ^/^ string "->" ^/^ pp_expr body)

and pp_dimension = function
  | Dimension_expr expr -> pp_expr expr
  | Dimension_splice id -> pp_id id ^^ string "..."

and pp_field { expr; alias } =
  match alias with
  | None -> pp_expr expr
  | Some alias_name -> group (pp_expr expr ^/^ string "AS" ^/^ pp_id alias_name)

and pp_from_one from_one =
  match from_one.node with
  | F_table { db; table; alias; final } ->
      let base = pp_id db ^^ string "." ^^ pp_id table in
      let final_part = if final then string " FINAL" else empty in
      group (base ^^ string " AS " ^^ pp_id alias ^^ final_part)
  | F_select { select; alias; cluster_name = Some cluster_name } ->
      let pp_cluster_name =
        match cluster_name with
        | Cluster_name id -> pp_id id
        | Cluster_name_param id -> string "?" ^^ pp_id id
      in
      group
        (string "cluster" ^^ lparen ^^ pp_cluster_name ^^ comma ^^ space
       ^^ string "view" ^^ lparen ^^ pp_query select ^^ rparen ^^ rparen
       ^^ space ^^ string "AS" ^^ space ^^ pp_id alias)
  | F_select { select; alias; cluster_name = None } ->
      group
        (lparen
        ^^ nest 2 (break 0 ^^ pp_query select ^^ rparen)
        ^^ group (string " AS" ^/^ pp_id alias))
  | F_value { id; alias } -> group (pp_id id ^^ string " AS " ^^ pp_id alias)

and pp_from from =
  match from.node with
  | F from_one -> pp_from_one from_one
  | F_join { kind; from; join; on; _ } ->
      let join_kind_str =
        match kind with
        | `INNER_JOIN -> "INNER JOIN"
        | `LEFT_JOIN -> "LEFT JOIN"
      in
      group
        (pp_from from ^/^ string join_kind_str ^/^ pp_from_one join
       ^/^ string "ON" ^/^ pp_expr on)

and pp_query { node; eq = _; loc = _ } =
  match node with
  | Q_union (q1, q2) -> group (pp_query q1 ^/^ string "UNION" ^/^ pp_query q2)
  | Q_param id -> string "?" ^^ pp_id id
  | Q_select
      {
        select;
        from;
        prewhere;
        where;
        qualify;
        group_by;
        having;
        order_by;
        limit;
        offset;
      } ->
      let pp_fields =
        match select with
        | Select_fields fields ->
            separate (string "," ^^ break 1) (List.map ~f:pp_field fields)
        | Select_splice id -> pp_id id ^^ string "..."
      in
      let select = group (string "SELECT" ^^ nest 2 (break 1 ^^ pp_fields)) in
      let from = group (string "FROM " ^^ pp_from from) in
      let prewhere =
        match prewhere with
        | None -> None
        | Some expr -> Some (group (string "PREWHERE" ^/^ pp_expr expr))
      in
      let where =
        match where with
        | None -> None
        | Some expr -> Some (group (string "WHERE" ^/^ pp_expr expr))
      in
      let qualify =
        match qualify with
        | None -> None
        | Some expr -> Some (group (string "QUALIFY" ^/^ pp_expr expr))
      in
      let group_by =
        match group_by with
        | None -> None
        | Some dimensions ->
            let dimensions =
              match dimensions with
              | [] -> string "()"
              | dimensions ->
                  separate (string ", ") (List.map ~f:pp_dimension dimensions)
            in
            Some (group (string "GROUP BY " ^^ dimensions))
      in
      let having =
        match having with
        | None -> None
        | Some expr -> Some (group (string "HAVING" ^/^ pp_expr expr))
      in
      let order_by =
        match order_by with
        | None -> None
        | Some items ->
            let pp_item = function
              | Syntax.Order_by_splice id -> pp_id id ^^ string "..."
              | Syntax.Order_by_expr (expr, dir) ->
                  let dir = match dir with `ASC -> "ASC" | `DESC -> "DESC" in
                  group (pp_expr expr ^^ string " " ^^ string dir)
            in
            let pp_items = separate (string ", ") (List.map ~f:pp_item items) in
            Some (group (string "ORDER BY " ^^ pp_items))
      in
      let limit =
        match limit with
        | None -> None
        | Some expr -> Some (group (string "LIMIT" ^/^ pp_expr expr))
      in
      let offset =
        match offset with
        | None -> None
        | Some expr -> Some (group (string "OFFSET" ^/^ pp_expr expr))
      in
      group
        (separate (break 1)
           (List.filter_map ~f:Fun.id
              [
                Some select;
                Some from;
                prewhere;
                where;
                qualify;
                group_by;
                having;
                order_by;
                limit;
                offset;
              ]))

and pp_typ typ =
  match typ.node with
  | T id -> pp_id id
  | T_app (id, args) ->
      let pp_args =
        match args with
        | [] -> empty
        | _ -> separate (string "," ^^ space) (List.map ~f:pp_typ args)
      in
      pp_id id ^^ string "(" ^^ pp_args ^^ string ")"

let print' pp v =
  let buffer = Buffer.create 256 in
  ToBuffer.pretty 1.0 80 buffer (pp v);
  Buffer.contents buffer

let print_expr = print' pp_expr
let print_query = print' pp_query
let print_from_one = print' pp_from_one
