%{
  open Syntax
  
  let make_loc start_pos end_pos = 
    { Loc.start_pos; end_pos }
  
  let with_loc start_pos end_pos node =
    { Loc.node; loc = make_loc start_pos end_pos }
%}

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
      | items -> with_loc $startpos $endpos (E_concat items)
    }

uexpr_items:
    { [] }
  | item=uexpr_item rest=uexpr_items { item :: rest }

uexpr_item:
    param=PARAM { 
      let id = with_loc $startpos $endpos param in
      with_loc $startpos $endpos (E_value (id, None)) 
    }
  | sql=SQL { 
      let sql = with_loc $startpos $endpos sql in
      with_loc $startpos $endpos (E_id sql) 
    }
