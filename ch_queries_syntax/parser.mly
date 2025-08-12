%{
  open Syntax
  
  let make_loc start_pos end_pos = 
    { Loc.start_pos; end_pos }
  
  let make_expr start_pos end_pos node = make_expr ~loc:(make_loc start_pos end_pos) node
  let make_typ start_pos end_pos node = make_typ ~loc:(make_loc start_pos end_pos) node
  let make_query start_pos end_pos node = make_query ~loc:(make_loc start_pos end_pos) node
  let make_id start_pos end_pos node = make_id ~loc:(make_loc start_pos end_pos) node
  let make_from_one start_pos end_pos node = make_from_one ~loc:(make_loc start_pos end_pos) node
  let make_from start_pos end_pos node = make_from ~loc:(make_loc start_pos end_pos) node
%}

%token <string> ID
%token <string> PARAM
%token <string> PARAM_SPLICE
%token <string> CH_PARAM
%token <string> OCAML_EXPR
%token <string> STRING
%token <int> NUMBER
%token TRUE FALSE
%token SELECT FROM PREWHERE WHERE AS DOT COLON
%token LPAREN RPAREN LBRACKET RBRACKET COMMA
%token PLUS MINUS STAR SLASH EQUALS GT LT GE LE
%token AND OR NOT
%token INNER JOIN LEFT OPTIONAL ON
%token GROUP BY HAVING ORDER ASC DESC
%token OVER PARTITION QUALIFY
%token LIMIT OFFSET
%token CLUSTER VIEW FINAL
%token IN
%token UNION
%token SETTINGS
%token ARROW
%token EOF

%left UNION
%right ARROW
%left OR
%left AND
%right NOT
%left EQUALS GT LT GE LE
%left PLUS MINUS
%left STAR SLASH
%right UMINUS  (* unary minus *)
%left IN

%start a_query a_expr a_typ
%type <Syntax.query> a_query
%type <Syntax.expr> a_expr
%type <Syntax.typ> a_typ

%%

a_typ:
    t=typ EOF { t }

typ:
    id=id
    { make_typ $startpos $endpos (T id) }
  | id=id LPAREN args=separated_list(COMMA, typ) RPAREN
    { make_typ $startpos $endpos (T_app (id, args)) }

a_query:
    q=query EOF { q }

query_no_param:
    SELECT select=select FROM from=from prewhere=prewhere? where=where? qualify=qualify? group_by=group_by? having=having? order_by=order_by? limit=limit? offset=offset? settings=settings?
    { make_query $startpos $endpos (Syntax.Q_select { select; from; prewhere; where; qualify; group_by; having; order_by; limit; offset; settings = Option.value settings ~default:[] }) }
  | q1=query UNION q2=query
    { make_query $startpos $endpos (Syntax.Q_union (q1, q2)) }

query:
    q=query_no_param { q }
  | param=param { make_query $startpos $endpos (Syntax.Q_param param) }

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
    id=ID { make_id $startpos $endpos id }

param:
    id=PARAM { make_id $startpos $endpos id }

param_splice:
    id=PARAM_SPLICE { make_id $startpos $endpos id }

alias:
    AS id=id { id }

alias_or_q:
    { make_id $startpos $endpos "q" }
  | AS id=id { id }

prewhere:
    PREWHERE e=expr { e }

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

settings:
    SETTINGS items=separated_list(COMMA, setting_item) { items }

setting_item:
    id=id EQUALS value=setting_value { Setting_item (id, value) }
  | splice=param_splice { Setting_splice splice }

setting_value:
    lit=setting_literal { Setting_lit lit }
  | param=param { Setting_param param }

setting_literal:
    n=NUMBER { L_int n }
  | s=STRING { L_string s }
  | TRUE     { L_bool true }
  | FALSE    { L_bool false }

window_spec:
    partition_by=partition_by? order_by=order_by?
    { { partition_by; order_by } }

partition_by:
    PARTITION BY dimensions=separated_list(COMMA, dimension) { dimensions }

from:
    f=from_one
    { make_from $startpos $endpos (F f) }
  | from=from kind=join_kind join=from_one ON on=expr
    { make_from $startpos $endpos (F_join { kind; from; join; on }) }

from_one:
    id=param alias=alias? final=final { make_from_one $startpos $endpos (F_param {id; alias = Option.value alias ~default:id; final;}) }
  | id=id alias=alias? final=final { make_from_one $startpos $endpos (F_param {id; alias = Option.value alias ~default:id; final;}) }
  | db=id DOT table=id alias=alias? final=final
    { make_from_one $startpos $endpos (F_table { db; table; alias = Option.value alias ~default:table; final; }) }
  | LPAREN q=query RPAREN alias=alias_or_q
    { make_from_one $startpos $endpos (F_select { select = q; alias; cluster_name = None }) }
  | CLUSTER LPAREN cluster_name=cluster_name COMMA VIEW LPAREN q=query RPAREN RPAREN alias=alias_or_q
    { make_from_one $startpos $endpos (F_select { select = q; alias; cluster_name = Some cluster_name }) }

cluster_name:
    id=id { Cluster_name id }
  | param=param { Cluster_name_param param }
  | id=CH_PARAM { Cluster_name (make_id $startpos $endpos id) }

final:
    { false }
  | FINAL { true }

join_kind:
          JOIN { `INNER_JOIN }
  | INNER JOIN { `INNER_JOIN }
  | LEFT  JOIN { `LEFT_JOIN }
  | LEFT  JOIN OPTIONAL { `LEFT_JOIN_OPTIONAL }

expr:
    id=id
    { make_expr $startpos $endpos (E_id id) }
  | ns=id DOT id=id
    { make_expr $startpos $endpos (E_col (ns, id)) }
  | n=NUMBER
    { make_expr $startpos $endpos (E_lit (L_int n)) }
  | s=STRING
    { make_expr $startpos $endpos (E_lit (L_string s)) }
  | TRUE
    { make_expr $startpos $endpos (E_lit (L_bool true)) }
  | FALSE
    { make_expr $startpos $endpos (E_lit (L_bool false)) }
  | LPAREN e=expr RPAREN
    { e }
  | LBRACKET es=separated_list(COMMA, expr) RBRACKET
    { make_expr $startpos $endpos (E_call (Func (make_id $startpos $endpos "["), es)) }
  | e1=expr PLUS e2=expr
    { make_expr $startpos $endpos (E_call (Func (make_id $startpos($2) $endpos($2) "+"), [e1; e2])) }
  | e1=expr MINUS e2=expr
    { make_expr $startpos $endpos (E_call (Func (make_id $startpos($2) $endpos($2) "-"), [e1; e2])) }
  | e1=expr STAR e2=expr
    { make_expr $startpos $endpos (E_call (Func (make_id $startpos($2) $endpos($2) "*"), [e1; e2])) }
  | e1=expr SLASH e2=expr
    { make_expr $startpos $endpos (E_call (Func (make_id $startpos($2) $endpos($2) "/"), [e1; e2])) }
  | e1=expr AND e2=expr
    { make_expr $startpos $endpos (E_call (Func (make_id $startpos($2) $endpos($2) "AND"), [e1; e2])) }
  | e1=expr OR e2=expr
    { make_expr $startpos $endpos (E_call (Func (make_id $startpos($2) $endpos($2) "OR"), [e1; e2])) }
  | NOT e=expr
    { make_expr $startpos $endpos (E_call (Func (make_id $startpos($1) $endpos($1) "NOT"), [e])) }
  | MINUS e=expr %prec UMINUS
    { make_expr $startpos $endpos (E_call (Func (make_id $startpos($1) $endpos($1) "-"), [e])) }
  | e1=expr EQUALS e2=expr
    { make_expr $startpos $endpos (E_call (Func (make_id $startpos($2) $endpos($2) "="), [e1; e2])) }
  | e1=expr GT e2=expr
    { make_expr $startpos $endpos (E_call (Func (make_id $startpos($2) $endpos($2) ">"), [e1; e2])) }
  | e1=expr LT e2=expr
    { make_expr $startpos $endpos (E_call (Func (make_id $startpos($2) $endpos($2) "<"), [e1; e2])) }
  | e1=expr GE e2=expr
    { make_expr $startpos $endpos (E_call (Func (make_id $startpos($2) $endpos($2) ">="), [e1; e2])) }
  | e1=expr LE e2=expr
    { make_expr $startpos $endpos (E_call (Func (make_id $startpos($2) $endpos($2) "<="), [e1; e2])) }
  | fn=id LPAREN args=separated_list(COMMA, expr) RPAREN
    { make_expr $startpos $endpos (E_call (Func fn, args)) }
  | table=id DOT method_name=id LPAREN args=separated_list(COMMA, expr) RPAREN
    { make_expr $startpos $endpos (E_call (Func_method (table, method_name), args)) }
  | fn=id LPAREN args=separated_list(COMMA, expr) RPAREN OVER LPAREN window_spec=window_spec RPAREN
    { make_expr $startpos $endpos (E_window (fn, args, window_spec)) }
  | param=param
    { make_expr $startpos $endpos (E_param (param, None)) }
  | param=param COLON typ=typ
    { make_expr $startpos $endpos (E_param (param, Some typ)) }
  | ocaml_expr=OCAML_EXPR
    { make_expr $startpos $endpos (E_ocaml_expr ocaml_expr) }
  | param_type=CH_PARAM
    { make_expr $startpos $endpos (E_unsafe (make_id $startpos $endpos param_type)) }
  | e=expr IN LPAREN q=query_no_param RPAREN
    { make_expr $startpos $endpos (E_in (e, In_query q)) }
  | e=expr IN e_rhs=expr
    { make_expr $startpos $endpos (E_in (e, In_expr e_rhs)) }
  | param=id ARROW body=expr
    { make_expr $startpos $endpos (E_lambda (param, body)) }
