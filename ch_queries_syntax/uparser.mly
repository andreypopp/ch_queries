%{
  open Syntax

  let make_loc start_pos end_pos =
    { Loc.start_pos; end_pos }

  let make_id start_pos end_pos node = make_id ~loc:(make_loc start_pos end_pos) node
  let make_expr start_pos end_pos node = make_expr ~loc:(make_loc start_pos end_pos) node

  let is_whitespace c =
    Char.equal c ' ' || Char.equal c '\t' || Char.equal c '\n' || Char.equal c '\r'

  let is_all_whitespace s =
    String.for_all ~f:is_whitespace s

  let trim_leading_whitespace items =
    match items with
    | [] -> []
    | first :: rest ->
      match first.node with
      | E_unsafe id ->
        let trimmed = String.ltrim id.node in
        if String.equal trimmed "" then rest
        else
          let new_id = Syntax.make_id ~loc:id.loc trimmed in
          let new_expr = Syntax.make_expr ~loc:first.loc (E_unsafe new_id) in
          new_expr :: rest
      | _ -> items

  let trim_trailing_whitespace items =
    let rec trim = function
      | [] -> []
      | [last] ->
        (match last.node with
        | E_unsafe id when is_all_whitespace id.node -> []
        | E_unsafe id ->
          let trimmed = String.rtrim id.node in
          let new_id = Syntax.make_id ~loc:id.loc trimmed in
          let new_expr = Syntax.make_expr ~loc:last.loc (E_unsafe new_id) in
          [new_expr]
        | _ -> [last])
      | x :: xs -> x :: trim xs
    in
    trim items

  let trim_whitespace items =
    items |> trim_leading_whitespace |> trim_trailing_whitespace
%}

%token <string * string> COLUMN
%token <string> PARAM
%token <string> SQL
%token EOF

%start a_uexpr
%type <Syntax.expr> a_uexpr

%%

a_uexpr:
    e=uexpr EOF { e }

uexpr:
    items=uexpr_items {
      let items = trim_whitespace items in
      match items with
      | [] -> make_expr $startpos $endpos (E_unsafe (make_id $startpos $endpos ""))
      | [item] -> item
      | items -> make_expr $startpos $endpos (E_unsafe_concat items)
    }

uexpr_items:
    { [] }
  | item=uexpr_item rest=uexpr_items { item :: rest }

uexpr_item:
    param=PARAM {
      let param = make_id $startpos $endpos param in
      make_expr $startpos $endpos (E_param {param; param_has_scope=false; param_optional=false})
    }
  | col=COLUMN { 
      let x, y = col in
      let x = make_id $startpos $endpos x in
      let y = make_id $startpos $endpos y in
      make_expr $startpos $endpos (E_col (x, y)) 
    }
  | sql=SQL { 
      let sql = make_id $startpos $endpos sql in
      make_expr $startpos $endpos (E_unsafe sql) 
    }
