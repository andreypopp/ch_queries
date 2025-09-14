open Printf
open PPrint
open Syntax

let pp_id id = string id.node

let get_precedence op arity =
  match (op, arity) with
  | "OR", 2 -> 2
  | "AND", 2 -> 3
  | "NOT", 1 -> 4
  | ("=" | ">" | "<" | ">=" | "<=" | "!=" | "IN"), 2 -> 5
  | ("+" | "-"), 2 -> 6
  | ("*" | "/"), 2 -> 7
  | "-", 1 -> 8 (* unary minus has high precedence *)
  | _ -> 10 (* atoms: literals, function calls, etc. *)

let escape_single_quoted s =
  let buf = Buffer.create (String.length s + 10) in
  String.iter s ~f:(function
    | '\'' -> Buffer.add_string buf "\\'"
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\t' -> Buffer.add_string buf "\\t"
    | '\r' -> Buffer.add_string buf "\\r"
    | c -> Buffer.add_char buf c);
  Buffer.contents buf

type opts = {
  force_parens : bool;
      (* Force parentheses around expressions, even if they are not needed following precedence rules. *)
}

let rec pp_expr opts ~parent_prec expr =
  match expr.node with
  | E_ascribe (expr, typ) ->
      group
        (pp_expr opts ~parent_prec expr
        ^^ space ^^ string ":" ^^ space ^^ pp_typ typ)
  | E_unsafe_concat xs ->
      group (separate_map empty (pp_expr opts ~parent_prec:0) xs)
  | E_unsafe id -> string id.node
  | E_id id -> pp_id id
  | E_col (ns, id) -> string (Printf.sprintf "%s.%s" ns.node id.node)
  | E_query (ns, expr) ->
      group
        (string (Printf.sprintf "%s." ns.node)
        ^^ parens (pp_expr opts ~parent_prec:0 expr))
  | E_lit (L_int n) -> string (string_of_int n)
  | E_lit (L_float n) -> string (string_of_float n)
  | E_lit L_null -> string "NULL"
  | E_lit (L_bool b) -> string (string_of_bool b)
  | E_lit (L_string s) ->
      string (Printf.sprintf "'%s'" (escape_single_quoted s))
  | E_lit (L_interval (n, unit)) ->
      let unit_str =
        match unit with
        | Year -> "YEARS"
        | Month -> "MONTHS"
        | Week -> "WEEKS"
        | Day -> "DAYS"
        | Hour -> "HOURS"
        | Minute -> "MINUTES"
        | Second -> "SECONDS"
      in
      string (Printf.sprintf "INTERVAL %d %s" n unit_str)
  | E_param v -> string (Printf.sprintf "$%s" v.node)
  | E_window (name, args, window_spec) ->
      let pp_args =
        match args with
        | [] -> empty
        | _ ->
            separate
              (string "," ^^ space)
              (List.map ~f:(pp_expr opts ~parent_prec:0) args)
      in
      let pp_partition_by =
        match window_spec.partition_by with
        | None -> empty
        | Some dimensions ->
            string "PARTITION BY" ^^ space
            ^^ separate
                 (string "," ^^ space)
                 (List.map ~f:(pp_dimension opts) dimensions)
      in
      let pp_order_by =
        match window_spec.order_by with
        | None -> empty
        | Some orders ->
            let pp_order = function
              | Order_by_expr (expr, `ASC) ->
                  pp_expr opts ~parent_prec:0 expr ^^ space ^^ string "ASC"
              | Order_by_expr (expr, `DESC) ->
                  pp_expr opts ~parent_prec:0 expr ^^ space ^^ string "DESC"
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
          let parens_if_needed content =
            if opts.force_parens then group (parens (content 0))
            else
              let prec = get_precedence name.node (List.length args) in
              if prec < parent_prec then group (parens (content prec))
              else group (content prec)
          in
          match (name.node, args) with
          | "+", [ left; right ] ->
              parens_if_needed @@ fun prec ->
              pp_expr opts ~parent_prec:prec left
              ^/^ string "+"
              ^/^ pp_expr opts ~parent_prec:prec right
          | "[", xs ->
              brackets
                (List.map ~f:(pp_expr opts ~parent_prec:0) xs
                |> separate (string "," ^^ space))
          | "-", [ left; right ] ->
              parens_if_needed @@ fun prec ->
              pp_expr opts ~parent_prec:prec left
              ^/^ string "-"
              (* Right operand gets prec+1 to enforce left associativity: a-b-c not a-(b-c) *)
              ^/^ pp_expr opts ~parent_prec:(prec + 1) right
          | "*", [ left; right ] ->
              parens_if_needed @@ fun prec ->
              pp_expr opts ~parent_prec:prec left
              ^/^ string "*"
              ^/^ pp_expr opts ~parent_prec:prec right
          | "/", [ left; right ] ->
              parens_if_needed @@ fun prec ->
              pp_expr opts ~parent_prec:prec left
              ^/^ string "/"
              (* Right operand gets prec+1 to enforce left associativity: a/b/c not a/(b/c) *)
              ^/^ pp_expr opts ~parent_prec:(prec + 1) right
          | "AND", [ left; right ] ->
              parens_if_needed @@ fun prec ->
              pp_expr opts ~parent_prec:prec left
              ^/^ string "AND"
              ^/^ pp_expr opts ~parent_prec:prec right
          | "OR", [ left; right ] ->
              parens_if_needed @@ fun prec ->
              pp_expr opts ~parent_prec:prec left
              ^/^ string "OR"
              ^/^ pp_expr opts ~parent_prec:prec right
          | "LIKE", [ left; right ] ->
              parens_if_needed @@ fun prec ->
              pp_expr opts ~parent_prec:prec left
              ^/^ string "LIKE"
              ^/^ pp_expr opts ~parent_prec:prec right
          | "NOT", [ operand ] ->
              parens_if_needed @@ fun prec ->
              string "NOT" ^/^ pp_expr opts ~parent_prec:prec operand
          | "-", [ operand ] ->
              parens_if_needed @@ fun prec ->
              string "-" ^^ pp_expr opts ~parent_prec:prec operand
          | "=", [ left; right ] ->
              parens_if_needed @@ fun prec ->
              pp_expr opts ~parent_prec:prec left
              ^/^ string "="
              ^/^ pp_expr opts ~parent_prec:prec right
          | ">", [ left; right ] ->
              parens_if_needed @@ fun prec ->
              pp_expr opts ~parent_prec:prec left
              ^/^ string ">"
              ^/^ pp_expr opts ~parent_prec:prec right
          | "<", [ left; right ] ->
              parens_if_needed @@ fun prec ->
              pp_expr opts ~parent_prec:prec left
              ^/^ string "<"
              ^/^ pp_expr opts ~parent_prec:prec right
          | ">=", [ left; right ] ->
              parens_if_needed @@ fun prec ->
              pp_expr opts ~parent_prec:prec left
              ^/^ string ">="
              ^/^ pp_expr opts ~parent_prec:prec right
          | "<=", [ left; right ] ->
              parens_if_needed @@ fun prec ->
              pp_expr opts ~parent_prec:prec left
              ^/^ string "<="
              ^/^ pp_expr opts ~parent_prec:prec right
          | "!=", [ left; right ] ->
              parens_if_needed @@ fun prec ->
              pp_expr opts ~parent_prec:prec left
              ^/^ string "!="
              ^/^ pp_expr opts ~parent_prec:prec right
          | "map_get", [ key; value ] ->
              pp_expr opts ~parent_prec:0 key
              ^^ brackets (pp_expr opts ~parent_prec:0 value)
          | _, _ ->
              let pp_args =
                match args with
                | [] -> empty
                | _ ->
                    separate
                      (string "," ^^ space)
                      (List.map ~f:(pp_expr opts ~parent_prec:0) args)
              in
              group (pp_id name ^^ string "(" ^^ pp_args ^^ string ")"))
      | Func_method (table, method_name) ->
          let pp_args =
            match args with
            | [] -> empty
            | _ ->
                separate
                  (string "," ^^ space)
                  (List.map ~f:(pp_expr opts ~parent_prec:0) args)
          in
          group
            (pp_id table ^^ string "." ^^ pp_id method_name ^^ string "("
           ^^ pp_args ^^ string ")"))
  | E_ocaml_expr s -> string ("?{" ^ s ^ "}")
  | E_in (expr, in_query) ->
      let prec = get_precedence "IN" 2 in
      let needs_parens = prec < parent_prec in
      let content =
        match in_query with
        | In_query query ->
            pp_expr opts ~parent_prec:prec expr
            ^/^ string "IN" ^/^ lparen
            ^^ nest 2 (break 0 ^^ pp_query opts query ^^ rparen)
        | In_expr expr' ->
            pp_expr opts ~parent_prec:prec expr
            ^/^ string "IN"
            ^/^ pp_expr opts ~parent_prec:prec expr'
      in
      if needs_parens then group (parens content) else group content
  | E_lambda (param, body) ->
      (* we will always print lambdas with parentheses around them *)
      group
        (parens
           (pp_id param ^/^ string "->"
           ^/^ parens (pp_expr opts ~parent_prec:0 body)))

and pp_dimension opts = function
  | Dimension_expr expr -> pp_expr opts ~parent_prec:0 expr
  | Dimension_splice id -> pp_id id ^^ string "..."

and pp_field opts { expr; alias } =
  match alias with
  | None -> pp_expr opts ~parent_prec:0 expr
  | Some alias_name ->
      group (pp_expr opts ~parent_prec:0 expr ^^ pp_as alias_name)

and pp_as id = group (nest 2 (break 1 ^^ string (sprintf "AS %s" id.node)))

and pp_with_field opts with_field =
  match with_field with
  | Syntax.With_expr field -> pp_field opts field
  | Syntax.With_query (id, query, materialized) ->
      let materialized =
        if materialized then string " MATERIALIZED" else empty
      in
      group
        (pp_id id ^^ string " AS" ^^ materialized ^^ string " ("
        ^^ nest 2 (break 0 ^^ pp_query opts query)
        ^^ break 0 ^^ string ")")

and pp_from_one opts from_one =
  match from_one.node with
  | F_table { db; table; alias; final } ->
      let base = pp_id db ^^ string "." ^^ pp_id table in
      let final = if final then string " FINAL" else empty in
      group (base ^^ pp_as alias ^^ final)
  | F_select { select; alias; cluster_name = Some cluster_name } ->
      let pp_cluster_name =
        match cluster_name with
        | Cluster_name id -> pp_id id
        | Cluster_name_param id -> string "?" ^^ pp_id id
      in
      group
        (string "cluster" ^^ lparen ^^ pp_cluster_name ^^ comma ^^ space
       ^^ string "view" ^^ lparen
        ^^ nest 2 (break 0 ^^ pp_query opts select)
        ^^ rparen ^^ rparen ^^ pp_as alias)
  | F_select { select; alias; cluster_name = None } ->
      group
        (lparen
        ^^ nest 2 (break 0 ^^ pp_query opts select ^^ rparen)
        ^^ pp_as alias)
  | F_param { id; alias; final } ->
      let final = if final then string " FINAL" else empty in
      group (pp_id id ^^ pp_as alias ^^ final)
  | F_ascribe (from, typ) ->
      group (pp_from_one opts from ^^ string "::" ^^ pp_typ typ)

and pp_from opts from =
  match from.node with
  | F from_one -> pp_from_one opts from_one
  | F_join { kind; from; join; on; _ } ->
      let join_kind_str =
        match kind with
        | `INNER_JOIN -> "INNER JOIN"
        | `LEFT_JOIN -> "LEFT JOIN"
        | `LEFT_JOIN_OPTIONAL -> "LEFT JOIN OPTIONAL"
      in
      group
        (pp_from opts from
        ^/^ group (string join_kind_str ^/^ pp_from_one opts join)
        ^/^ group (string "ON" ^/^ pp_expr opts ~parent_prec:0 on))

and pp_query opts { node; eq = _; loc = _ } =
  match node with
  | Q_union (q1, q2) ->
      group (pp_query opts q1 ^/^ string "UNION" ^/^ pp_query opts q2)
  | Q_param id -> string "?" ^^ pp_id id
  | Q_ascribe (q, t) -> pp_query opts q ^^ string "::" ^^ pp_typ t
  | Q_select
      {
        with_fields;
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
        settings;
      } ->
      let pp_fields =
        match select with
        | Select_fields fields ->
            separate
              (string "," ^^ break 1)
              (List.map ~f:(pp_field opts) fields)
        | Select_splice id -> pp_id id ^^ string "..."
      in
      let with_clause =
        match with_fields with
        | [] -> None
        | fields ->
            let pp_with_fields =
              separate
                (string "," ^^ break 1)
                (List.map ~f:(pp_with_field opts) fields)
            in
            Some (group (string "WITH" ^^ nest 2 (break 1 ^^ pp_with_fields)))
      in
      let select = group (string "SELECT" ^^ nest 2 (break 1 ^^ pp_fields)) in
      let from = group (string "FROM " ^^ pp_from opts from) in
      let prewhere =
        match prewhere with
        | None -> None
        | Some expr ->
            Some
              (group (string "PREWHERE" ^/^ pp_expr opts ~parent_prec:0 expr))
      in
      let where =
        match where with
        | None -> None
        | Some expr ->
            Some (group (string "WHERE" ^/^ pp_expr opts ~parent_prec:0 expr))
      in
      let qualify =
        match qualify with
        | None -> None
        | Some expr ->
            Some (group (string "QUALIFY" ^/^ pp_expr opts ~parent_prec:0 expr))
      in
      let group_by =
        match group_by with
        | None -> None
        | Some dimensions ->
            let dimensions =
              match dimensions with
              | [] -> string "()"
              | dimensions ->
                  separate (string ", ")
                    (List.map ~f:(pp_dimension opts) dimensions)
            in
            Some (group (string "GROUP BY " ^^ dimensions))
      in
      let having =
        match having with
        | None -> None
        | Some expr ->
            Some (group (string "HAVING" ^/^ pp_expr opts ~parent_prec:0 expr))
      in
      let order_by =
        match order_by with
        | None -> None
        | Some items ->
            let pp_item = function
              | Syntax.Order_by_splice id -> pp_id id ^^ string "..."
              | Syntax.Order_by_expr (expr, dir) ->
                  let dir = match dir with `ASC -> "ASC" | `DESC -> "DESC" in
                  group
                    (pp_expr opts ~parent_prec:0 expr
                    ^^ string " " ^^ string dir)
            in
            let pp_items = separate (string ", ") (List.map ~f:pp_item items) in
            Some (group (string "ORDER BY " ^^ pp_items))
      in
      let limit =
        match limit with
        | None -> None
        | Some expr ->
            Some (group (string "LIMIT" ^/^ pp_expr opts ~parent_prec:0 expr))
      in
      let offset =
        match offset with
        | None -> None
        | Some expr ->
            Some (group (string "OFFSET" ^/^ pp_expr opts ~parent_prec:0 expr))
      in
      let settings =
        match settings with
        | [] -> None
        | items ->
            let pp_setting_item = function
              | Setting_item (id, value) ->
                  let pp_value =
                    match value with
                    | Setting_lit (L_int n) -> string (string_of_int n)
                    | Setting_lit (L_string s) ->
                        string (Printf.sprintf "'%s'" (escape_single_quoted s))
                    | Setting_lit (L_float n) -> string (string_of_float n)
                    | Setting_lit (L_bool b) -> string (if b then "1" else "0")
                    | Setting_lit L_null -> string "NULL"
                    | Setting_lit (L_interval (n, unit)) ->
                        let unit_str =
                          match unit with
                          | Year -> "YEARS"
                          | Month -> "MONTHS"
                          | Week -> "WEEKS"
                          | Day -> "DAYS"
                          | Hour -> "HOURS"
                          | Minute -> "MINUTES"
                          | Second -> "SECONDS"
                        in
                        string (Printf.sprintf "INTERVAL %d %s" n unit_str)
                    | Setting_param param -> string "?" ^^ pp_id param
                  in
                  pp_id id ^^ string "=" ^^ pp_value
              | Setting_splice id -> string "?" ^^ pp_id id ^^ string "..."
            in
            let pp_items =
              separate (string ", ") (List.map ~f:pp_setting_item items)
            in
            Some (group (string "SETTINGS " ^^ pp_items))
      in
      group
        (separate (break 1)
           (List.filter_map ~f:Fun.id
              [
                with_clause;
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
                settings;
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
  | T_any -> string "Any"
  | T_custom ocaml_type -> string "Custom(" ^^ pp_id ocaml_type ^^ string ")"
  | T_db_table (db, table, `NON_NULL) ->
      string (sprintf "%s.%s" db.node table.node)
  | T_db_table (db, table, `NULL) ->
      string (sprintf "%s.%s?" db.node table.node)
  | T_scope (cols, is_open, nullable) -> (
      string "("
      ^^ pp_scope_columns ~is_open cols
      ^^ match nullable with `NON_NULL -> string ")" | `NULL -> string ")?")

and pp_scope_columns ~is_open cols =
  match (cols, is_open) with
  | [], `Closed -> empty
  | [], `Open -> string "..."
  | cols, `Closed -> pp_scope_columns' cols
  | cols, `Open -> pp_scope_columns' cols ^^ string ", ..."

and pp_scope_columns' cols =
  separate (string "," ^^ space) (List.map ~f:pp_scope_column cols)

and pp_scope_column col = pp_id col.name ^^ space ^^ pp_typ col.typ

let print' pp v =
  let buffer = Buffer.create 256 in
  ToBuffer.pretty 1.0 80 buffer (pp v);
  Buffer.contents buffer

let print_expr ?(force_parens = false) e =
  print' (pp_expr { force_parens } ~parent_prec:0) e

let print_query ?(force_parens = false) q = print' (pp_query { force_parens }) q
