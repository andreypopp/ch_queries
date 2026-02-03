%{
  open Syntax
  
  let make_loc start_pos end_pos = 
    { Loc.start_pos; end_pos }
  
  let make_id start_pos end_pos node = make_id ~loc:(make_loc start_pos end_pos) node
  let make_expr start_pos end_pos node = make_expr ~loc:(make_loc start_pos end_pos) node
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
      match items with
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
