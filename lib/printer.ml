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
  | E_call (name, args) -> (
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

and pp_query { Loc.node = { Syntax.fields; from; where }; _ } =
  let pp_fields =
    separate (string "," ^^ break 1) (List.map ~f:pp_field fields)
  in
  let select = group (string "SELECT" ^^ nest 2 (break 1 ^^ pp_fields)) in
  let from = group (string "FROM " ^^ pp_from from) in
  let where =
    match where with
    | None -> None
    | Some expr -> Some (group (string "WHERE" ^/^ pp_expr expr))
  in
  group
    (separate (break 1)
       (List.filter_map ~f:Fun.id [ Some select; Some from; where ]))

let print' pp v =
  let buffer = Buffer.create 256 in
  ToBuffer.pretty 1.0 80 buffer (pp v);
  Buffer.contents buffer

let print_expr = print' pp_expr
let print_query = print' pp_query
let print_from_one = print' pp_from_one
