%{
  open Syntax
  
  let make_loc start_pos end_pos = 
    { Loc.start_pos; end_pos }
  
  let with_loc start_pos end_pos node =
    { Loc.node; loc = make_loc start_pos end_pos }
%}

%token <string> ID
%token <string> PARAM
%token <string> PARAM_SPLICE
%token <string> OCAML_EXPR
%token <string> STRING
%token <int> NUMBER
%token TRUE FALSE
%token SELECT FROM WHERE AS DOT
%token LPAREN RPAREN COMMA
%token PLUS MINUS STAR SLASH EQUALS
%token AND OR
%token INNER JOIN LEFT ON
%token GROUP BY HAVING ORDER ASC DESC
%token OVER PARTITION QUALIFY
%token LIMIT OFFSET
%token CLUSTER VIEW FINAL
%token EOF

%left OR
%left AND
%left EQUALS
%left PLUS MINUS
%left STAR SLASH

%start a_query a_expr
%type <Syntax.query> a_query
%type <Syntax.expr> a_expr

%%

a_query:
    q=query EOF { q }

query:
    SELECT select=select FROM from=from where=where? qualify=qualify? group_by=group_by? having=having? order_by=order_by? limit=limit? offset=offset?
    { with_loc $startpos $endpos { select; from; where; qualify; group_by; having; order_by; limit; offset } }

a_expr:
    e=expr EOF { e }

select:
    id=param_splice
    { Select_splice id }
  | fs=fields
    { Select_fields fs }

fields:
    separated_list(COMMA, field) { $1 }

field:
    e=expr a=alias?
    { { expr = e; alias = a } }

id:
    id=ID { with_loc $startpos $endpos id }

param:
    id=PARAM { with_loc $startpos $endpos id }

param_splice:
    id=PARAM_SPLICE { with_loc $startpos $endpos id }

alias:
    AS id=id { id }

where:
    WHERE e=expr { e }

qualify:
    QUALIFY e=expr { e }

having:
    HAVING e=expr { e }

group_by:
    GROUP BY dimensions=separated_list(COMMA, dimension) { dimensions }

dimension:
    e=param_splice { Dimension_splice e }
  | e=expr { Dimension_expr e }

order_by:
    ORDER BY items=separated_list(COMMA, order_by_item) { items }

order_by_item:
    e=param_splice { Order_by_splice e }
  | e=expr { Order_by_expr (e, `ASC) }
  | e=expr ASC { Order_by_expr (e, `ASC) }
  | e=expr DESC { Order_by_expr (e, `DESC) }

limit:
    LIMIT e=expr { e }

offset:
    OFFSET e=expr { e }

window_spec:
    partition_by=partition_by? order_by=order_by?
    { { partition_by; order_by } }

partition_by:
    PARTITION BY dimensions=separated_list(COMMA, dimension) { dimensions }

from:
    f=from_one
    { with_loc $startpos $endpos (F f) }
  | from=from kind=join_kind join=from_one ON on=expr
    { with_loc $startpos $endpos (F_join { kind; from; join; on }) }

from_one:
    id=param alias=alias? { with_loc $startpos $endpos (F_value {id; alias = Option.value alias ~default:id}) }
  | db=id DOT table=id alias=alias? final=final?
    { with_loc $startpos $endpos (F_table { db; table; alias = Option.value alias ~default:table; final = Option.value final ~default:false }) }
  | LPAREN q=query RPAREN alias=alias
    { with_loc $startpos $endpos (F_select { select = q; alias; cluster_name = None }) }
  | CLUSTER LPAREN cluster_name=cluster_name COMMA VIEW LPAREN q=query RPAREN RPAREN alias=alias
    { with_loc $startpos $endpos (F_select { select = q; alias; cluster_name = Some cluster_name }) }

cluster_name:
    id=id { Cluster_name id }
  | param=param { Cluster_name_param param }

final:
    FINAL { true }

join_kind:
          JOIN { `INNER_JOIN }
  | INNER JOIN { `INNER_JOIN }
  | LEFT  JOIN { `LEFT_JOIN }

expr:
    ns=id DOT id=id
    { with_loc $startpos $endpos (E_col (ns, id)) }
  | n=NUMBER
    { with_loc $startpos $endpos (E_lit (L_int n)) }
  | s=STRING
    { with_loc $startpos $endpos (E_lit (L_string s)) }
  | TRUE
    { with_loc $startpos $endpos (E_lit (L_bool true)) }
  | FALSE
    { with_loc $startpos $endpos (E_lit (L_bool false)) }
  | LPAREN e=expr RPAREN
    { e }
  | e1=expr PLUS e2=expr
    { with_loc $startpos $endpos (E_call (Func (with_loc $startpos($2) $endpos($2) "+"), [e1; e2])) }
  | e1=expr MINUS e2=expr
    { with_loc $startpos $endpos (E_call (Func (with_loc $startpos($2) $endpos($2) "-"), [e1; e2])) }
  | e1=expr STAR e2=expr
    { with_loc $startpos $endpos (E_call (Func (with_loc $startpos($2) $endpos($2) "*"), [e1; e2])) }
  | e1=expr SLASH e2=expr
    { with_loc $startpos $endpos (E_call (Func (with_loc $startpos($2) $endpos($2) "/"), [e1; e2])) }
  | e1=expr AND e2=expr
    { with_loc $startpos $endpos (E_call (Func (with_loc $startpos($2) $endpos($2) "AND"), [e1; e2])) }
  | e1=expr OR e2=expr
    { with_loc $startpos $endpos (E_call (Func (with_loc $startpos($2) $endpos($2) "OR"), [e1; e2])) }
  | e1=expr EQUALS e2=expr
    { with_loc $startpos $endpos (E_call (Func (with_loc $startpos($2) $endpos($2) "="), [e1; e2])) }
  | fn=id LPAREN args=separated_list(COMMA, expr) RPAREN
    { with_loc $startpos $endpos (E_call (Func fn, args)) }
  | table=id DOT method_name=id LPAREN args=separated_list(COMMA, expr) RPAREN
    { with_loc $startpos $endpos (E_call (Func_method (table, method_name), args)) }
  | fn=id LPAREN args=separated_list(COMMA, expr) RPAREN OVER LPAREN window_spec=window_spec RPAREN
    { with_loc $startpos $endpos (E_window (fn, args, window_spec)) }
  | param=param
    { with_loc $startpos $endpos (E_value param) }
  | ocaml_expr=OCAML_EXPR
    { with_loc $startpos $endpos (E_ocaml_expr ocaml_expr) }
