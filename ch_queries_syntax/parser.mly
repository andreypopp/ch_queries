%{
  open Syntax

  let ascribe_query q t =
    match t with
    | None -> q
    | Some t ->
      let loc = { Loc.start_pos = q.loc.Loc.start_pos; end_pos = t.loc.Loc.end_pos } in
      make_query ~loc (Q_ascribe (q, t))

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
%token <Syntax.expr> UNSAFE
%token <string> STRING
%token <int> NUMBER
%token <float> FLOAT
%token TRUE FALSE
%token SELECT FROM PREWHERE WHERE AS DOT COLONCOLON
%token LPAREN RPAREN LBRACKET RBRACKET COMMA
%token PLUS MINUS STAR SLASH EQUALS GT LT GE LE NOT_EQUAL QUESTION
%token DOT_DOT_DOT
%token AND OR NOT LIKE
%token INNER JOIN LEFT OPTIONAL ON
%token GROUP BY HAVING ORDER ASC DESC
%token OVER PARTITION QUALIFY
%token LIMIT OFFSET
%token CLUSTER VIEW FINAL
%token IN
%token UNION
%token SETTINGS
%token INTERVAL
%token YEAR MONTH WEEK DAY HOUR MINUTE SECOND
%token ARROW
%token WITH
%token FILL STEP TO INTERPOLATE
%token AS_MATERIALIZED AS_LPAREN
%token <string> AS_PARAM
%token EOF

%left UNION
%right ARROW
%left OR
%left AND
%right NOT
%nonassoc LIKE
%left EQUALS GT LT GE LE NOT_EQUAL IN
%left PLUS MINUS
%left STAR SLASH
%right UMINUS  (* unary minus *)
%left LBRACKET
%left COLONCOLON

%start a_query a_expr a_typ a_from a_scope_columns
%type <Syntax.query> a_query
%type <Syntax.expr> a_expr
%type <Syntax.typ> a_typ
%type <Syntax.from> a_from
%type <Syntax.scope_column list * [`Closed | `Open]> a_scope_columns

%%

a_typ:
    t=typ EOF { t }

typ:
    id=id { 
      let t =
        match id.node with
        | "Any" -> T_any
        | _ -> T id
      in
      make_typ $startpos $endpos t }
  | id=id LPAREN args=flex_list(COMMA, typ) RPAREN { 
      let t =
        match id.node, args with
        | "Custom", [{node=T id;_}] -> T_custom id
        | _ -> T_app (id, args)
      in
      make_typ $startpos $endpos t }
  | t = scope_typ { t }

%inline scope_typ:
    LPAREN columns=scope_columns RPAREN nullable=scope_nullable
    { let cols, is_open = columns in make_typ $startpos $endpos (T_scope (cols, is_open, nullable)) }
  | db=id DOT table=id nullable=scope_nullable { make_typ $startpos $endpos (T_db_table (db, table, nullable)) }
  | DOT_DOT_DOT nullable=scope_nullable { make_typ $startpos $endpos (T_scope ([], `Open, nullable)) }

scope_nullable:
    QUESTION { `NULL }
  | { `NON_NULL }

scope_columns:
    { [], `Closed }
  | x = scope_column { [x], `Closed }
  | DOT_DOT_DOT { [], `Open }
  | x = scope_column; COMMA; xs = scope_columns { let xs, is_open = xs in x::xs, is_open }

scope_column:
  name=id typ=typ { {name;typ} }

a_query:
    q=query_ascribed EOF { q }

a_from:
    FROM f=from EOF { f }

query_select:
  with_fields=with_fields? SELECT select=select FROM from=from prewhere=prewhere? where=where? qualify=qualify? group_by=group_by? having=having? order_by=order_by? limit=limit? offset=offset? settings=settings?
  { make_query $startpos $endpos (Syntax.Q_select { with_fields = Option.value with_fields ~default:[]; select; from; prewhere; where; qualify; group_by; having; order_by; limit; offset; settings = Option.value settings ~default:[] }) }

with_fields:
  WITH xs=nonempty_flex_list(COMMA, with_field) { xs }

with_field:
    id=id AS_LPAREN q=query RPAREN t=query_ascription?
    { With_query (id, ascribe_query q t, false) }
  | id=id q=as_param t=query_ascription?
    { let q = make_query $startpos(q) $endpos(q) (Q_param q) in
      With_query (id, ascribe_query q t, false) }
  | id=id AS_MATERIALIZED LPAREN q=query RPAREN t=query_ascription?
    { With_query (id, ascribe_query q t, true) }
  | id=id AS_MATERIALIZED q=param t=query_ascription?
    { let q = make_query $startpos(q) $endpos(q) (Q_param q) in
      With_query (id, ascribe_query q t, true) }
  | expr=expr AS alias=id
    { With_expr { expr = expr; alias = Some alias } }

as_param:
    id=AS_PARAM { make_id $startpos $endpos id }

%inline query_no_param:
    q=query_select { q }
  | x=query UNION y=query { make_query $startpos $endpos (Syntax.Q_union (x, y)) }

query:
    q=query_no_param { q }
  | param=param { make_query $startpos $endpos (Syntax.Q_param param) }

query_ascribed:
    q=query { q }
  | LPAREN q=query RPAREN t=query_ascription { ascribe_query q (Some t) }
  | p=param t=query_ascription { ascribe_query (make_query $startpos $endpos (Syntax.Q_param p)) (Some t) }

%inline query_ascription:
  COLONCOLON t=typ { t }

a_expr:
    e=expr EOF { e }

a_scope_columns:
    cols=scope_columns EOF { cols }

select:
    id=param_splice
    { Select_splice id }
  | fs=fields
    { Select_fields fs }

fields:
    nonempty_flex_list(COMMA, field) { $1 }

field:
    e=expr a=alias?
    { { expr = e; alias = a } }

id:
    id=ID { make_id $startpos $endpos id }

alias:
    AS id=id { id }

alias_or_q:
    { make_id $startpos $endpos "q" }
  | id=alias { id }

param:
    id=PARAM { make_id $startpos $endpos id }

param_splice:
    id=PARAM_SPLICE { make_id $startpos $endpos id }

prewhere:
    PREWHERE e=expr { e }

where:
    WHERE e=expr { e }

qualify:
    QUALIFY e=expr { e }

having:
    HAVING e=expr { e }

group_by:
    GROUP BY dimensions=nonempty_flex_list(COMMA, dimension) { dimensions }

dimension:
    e=param_splice { Dimension_splice e }
  | e=expr { Dimension_expr e }

order_by:
    ORDER BY items=nonempty_flex_list(COMMA, order_by_item) { items }

order_by_item:
    e=param_splice { Order_by_splice e }
  | e=expr fill=with_fill { Order_by_expr (e, `ASC, fill) }
  | e=expr ASC fill=with_fill { Order_by_expr (e, `ASC, fill) }
  | e=expr DESC fill=with_fill { Order_by_expr (e, `DESC, fill) }

with_fill:
    { None }
  | WITH FILL from_=fill_from? to_=fill_to? step=fill_step? interpolate=interpolate_clause?
    { Some { fill_from = from_; fill_to = to_; fill_step = step;
             fill_interpolate = Option.value interpolate ~default:[] } }

fill_from: FROM e=expr { e }
fill_to: TO e=expr { e }
fill_step: STEP e=expr { e }

interpolate_clause:
    INTERPOLATE { [] }
  | INTERPOLATE LPAREN items=flex_list(COMMA, interpolate_item) RPAREN { items }

interpolate_item:
    col=id { { interpolate_col = col; interpolate_expr = None } }
  | col=id AS e=expr { { interpolate_col = col; interpolate_expr = Some e } }
  | col=id p=as_param {
      let e = make_expr $startpos(p) $endpos(p) (E_param p) in
      { interpolate_col = col; interpolate_expr = Some e } }

limit:
    LIMIT e=expr { e }

offset:
    OFFSET e=expr { e }

settings:
    SETTINGS items=nonempty_flex_list(COMMA, setting_item) { items }

setting_item:
    id=id EQUALS value=setting_value { Setting_item (id, value) }
  | splice=param_splice { Setting_splice splice }

setting_value:
    lit=setting_literal { Setting_lit lit }
  | param=param { Setting_param param }

setting_literal:
    n=NUMBER { L_int n }
  | f=FLOAT  { L_float f }
  | s=STRING { L_string s }
  | TRUE     { L_bool true }
  | FALSE    { L_bool false }
  | INTERVAL n=NUMBER unit=interval_unit { L_interval (n, unit) }

interval_unit:
    YEAR   { Year }
  | MONTH  { Month }
  | WEEK   { Week }
  | DAY    { Day }
  | HOUR   { Hour }
  | MINUTE { Minute }
  | SECOND { Second }

window_spec:
    partition_by=partition_by? order_by=order_by?
    { { partition_by; order_by } }

partition_by:
    PARTITION BY dimensions=nonempty_flex_list(COMMA, dimension) { dimensions }

from:
    f=from_one
    { make_from $startpos $endpos (F f) }
  | from=from kind=join_kind join=from_one ON on=expr
    { make_from $startpos $endpos (F_join { kind; from; join; on }) }

from_one:
    id=from_param t=query_ascription? alias=alias? final=final { 
    let f = make_from_one $startpos $endpos (F_param {id; alias = Option.value alias ~default:id; final;}) in
    let f = match t with None -> f | Some t -> make_from_one f.loc.start_pos f.loc.end_pos (F_ascribe (f, t)) in
    f }
  | db=id DOT table=id alias=alias? final=final
    { make_from_one $startpos $endpos (F_table { db; table; alias = Option.value alias ~default:table; final; }) }
  | LPAREN q=query RPAREN t=query_ascription? alias=alias_or_q
    { make_from_one $startpos $endpos (F_select { select = ascribe_query q t; alias; cluster_name = None }) }
  | CLUSTER LPAREN cluster_name=cluster_name COMMA VIEW LPAREN q=query RPAREN t1=query_ascription? RPAREN t2=typ? alias=alias_or_q
    { make_from_one $startpos $endpos (F_select { select = ascribe_query (ascribe_query q t1) t2; alias; cluster_name = Some cluster_name }) }

%inline from_param:
    id=param { id }
  | id=id { id } (* TODO(andreypopp): probably need to deprecate this syntax *)

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
    { if String.equal (String.uppercase_ascii id.node) "NULL" then
        make_expr $startpos $endpos (E_lit L_null)
      else
        make_expr $startpos $endpos (E_id id) 
    }
  | ns=id DOT id=id
    { make_expr $startpos $endpos (E_col (ns, id)) }
  | ns=id DOT LPAREN e=expr RPAREN
    { make_expr $startpos $endpos (E_query (ns, e)) }
  | n=NUMBER
    { make_expr $startpos $endpos (E_lit (L_int n)) }
  | f=FLOAT
    { make_expr $startpos $endpos (E_lit (L_float f)) }
  | s=STRING
    { make_expr $startpos $endpos (E_lit (L_string s)) }
  | TRUE
    { make_expr $startpos $endpos (E_lit (L_bool true)) }
  | FALSE
    { make_expr $startpos $endpos (E_lit (L_bool false)) }
  | INTERVAL n=NUMBER unit=interval_unit
    { make_expr $startpos $endpos (E_lit (L_interval (n, unit))) }
  | LPAREN e=expr RPAREN
    { e }
  | m=expr LBRACKET k=expr RBRACKET
    { make_expr $startpos $endpos (E_call (Func (make_id $startpos $endpos "map_get"), [m; k])) }
  | LBRACKET es=flex_list(COMMA, expr) RBRACKET
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
  | e1=expr LIKE e2=expr
    { make_expr $startpos $endpos (E_call (Func (make_id $startpos($2) $endpos($2) "LIKE"), [e1; e2])) }
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
  | e1=expr NOT_EQUAL e2=expr
    { make_expr $startpos $endpos (E_call (Func (make_id $startpos($2) $endpos($2) "!="), [e1; e2])) }
  | fn=id LPAREN args=flex_list(COMMA, expr) RPAREN
    { make_expr $startpos $endpos (E_call (Func fn, args)) }
  | table=id DOT method_name=id LPAREN args=flex_list(COMMA, expr) RPAREN
    { make_expr $startpos $endpos (E_call (Func_method (table, method_name), args)) }
  | fn=id LPAREN args=flex_list(COMMA, expr) RPAREN OVER LPAREN window_spec=window_spec RPAREN
    { make_expr $startpos $endpos (E_window (fn, args, window_spec)) }
  | param=param
    { make_expr $startpos $endpos (E_param param) }
  | ocaml_expr=OCAML_EXPR
    { make_expr $startpos $endpos (E_ocaml_expr ocaml_expr) }
  | ch_param=CH_PARAM
    { make_expr $startpos $endpos (E_unsafe (make_id $startpos $endpos ch_param)) }
  | expr=UNSAFE
    { expr }
  | e=expr IN LPAREN q=query_no_param RPAREN
    { make_expr $startpos $endpos (E_in (e, In_query q)) }
  | e=expr IN e_rhs=expr
    { make_expr $startpos $endpos (E_in (e, In_expr e_rhs)) }
  | param=id ARROW body=expr
    { make_expr $startpos $endpos (E_lambda (param, body)) }
  | e=expr COLONCOLON t=typ
    { make_expr $startpos $endpos (E_ascribe (e, t)) }

(* Utilities for flexible lists (and its non-empty version).

   A flexible list [flex_list(delim, X)] is the delimited with [delim] list of
   it [X] items where it is allowed to have a trailing [delim].

   A non-empty [nonempty_flex_list(delim, X)] version of flexible list is
   provided as well.

   From http://gallium.inria.fr/blog/lr-lists/

 *)

flex_list(delim, X):
    { [] }
  | x = X { [x] }
  | x = X; delim; xs = flex_list(delim, X) { x::xs }

nonempty_flex_list(delim, X):
    x = X { [x] }
  | x = X; delim; xs = flex_list(delim, X) { x::xs }
