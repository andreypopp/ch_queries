open PPrint
open Syntax

let pp_id id = string id.Loc.node

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
  match expr.Loc.node with
  | E_col (ns, id) -> string (Printf.sprintf "%s.%s" ns.Loc.node id.Loc.node)
  | E_lit (L_int n) -> string (string_of_int n)
  | E_lit (L_bool b) -> string (string_of_bool b)
  | E_lit (L_string s) ->
      string (Printf.sprintf "'%s'" (escape_single_quoted s))
  | E_value v -> string v.node
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
          match (name.Loc.node, args) with
          | "+", [ left; right ] ->
              group (pp_expr left ^/^ string "+" ^/^ pp_expr right)
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

and pp_dimension = function
  | Dimension_expr expr -> pp_expr expr
  | Dimension_splice id -> pp_id id ^^ string "..."

let pp_field { expr; alias } =
  match alias with
  | None -> pp_expr expr
  | Some alias_name -> group (pp_expr expr ^/^ string "AS" ^/^ pp_id alias_name)

let rec pp_from_one from_one =
  match from_one.Loc.node with
  | F_table { db; table; alias } ->
      group
        (pp_id db ^^ string "." ^^ pp_id table ^^ string " AS " ^^ pp_id alias)
  | F_select { select; alias } ->
      group
        (string "("
        ^^ nest 2 (break 0 ^^ pp_query select ^^ string ")")
        ^^ group (string " AS" ^/^ pp_id alias))
  | F_value { id; alias } -> group (pp_id id ^^ string " AS " ^^ pp_id alias)

and pp_from from =
  match from.Loc.node with
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

and pp_query
    {
      Loc.node =
        {
          Syntax.select;
          from;
          where;
          qualify;
          group_by;
          having;
          order_by;
          limit;
          offset;
        };
      _;
    } =
  let pp_fields =
    match select with
    | Select_fields fields ->
        separate (string "," ^^ break 1) (List.map ~f:pp_field fields)
    | Select_splice id -> pp_id id ^^ string "..."
  in
  let select = group (string "SELECT" ^^ nest 2 (break 1 ^^ pp_fields)) in
  let from = group (string "FROM " ^^ pp_from from) in
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
            where;
            qualify;
            group_by;
            having;
            order_by;
            limit;
            offset;
          ]))

let print' pp v =
  let buffer = Buffer.create 256 in
  ToBuffer.pretty 1.0 80 buffer (pp v);
  Buffer.contents buffer

let print_expr = print' pp_expr
let print_query = print' pp_query
let print_from_one = print' pp_from_one
