%{
  open Syntax
  
  let make_loc start_pos end_pos = 
    { Loc.start_pos; end_pos }
  
  let with_loc start_pos end_pos node =
    { Loc.node; loc = make_loc start_pos end_pos }
%}

%token <string> ID
%token <string> STRING
%token <int> NUMBER
%token TRUE FALSE
%token SELECT FROM WHERE AS DOT
%token LPAREN RPAREN COMMA
%token PLUS MINUS STAR SLASH EQUALS
%token AND OR
%token INNER JOIN LEFT ON
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
    SELECT fields=fields FROM from=from where=where?
    { with_loc $startpos $endpos { fields; from; where } }

a_expr:
    e=expr EOF { e }

fields:
    separated_list(COMMA, field) { $1 }

field:
    e=expr a=alias?
    { { expr = e; alias = a } }

id:
    id=ID { with_loc $startpos $endpos id }

alias:
    AS id=id { id }

where:
    WHERE e=expr { e }

from:
    f=from_one
    { with_loc $startpos $endpos (F f) }
  | from=from kind=join_kind join=from_one ON on=expr
    { with_loc $startpos $endpos (F_join { kind; from; join; on }) }

from_one:
    id=id alias=alias? { with_loc $startpos $endpos (F_value {id; alias = Option.value alias ~default:id}) }
  | db=id DOT table=id alias=alias?
    { with_loc $startpos $endpos (F_table { db; table; alias = Option.value alias ~default:table }) }
  | LPAREN q=query RPAREN alias=alias
    { with_loc $startpos $endpos (F_select { select = q; alias }) }

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
    { with_loc $startpos $endpos (E_call (with_loc $startpos($2) $endpos($2) "+", [e1; e2])) }
  | e1=expr MINUS e2=expr
    { with_loc $startpos $endpos (E_call (with_loc $startpos($2) $endpos($2) "-", [e1; e2])) }
  | e1=expr STAR e2=expr
    { with_loc $startpos $endpos (E_call (with_loc $startpos($2) $endpos($2) "*", [e1; e2])) }
  | e1=expr SLASH e2=expr
    { with_loc $startpos $endpos (E_call (with_loc $startpos($2) $endpos($2) "/", [e1; e2])) }
  | e1=expr AND e2=expr
    { with_loc $startpos $endpos (E_call (with_loc $startpos($2) $endpos($2) "AND", [e1; e2])) }
  | e1=expr OR e2=expr
    { with_loc $startpos $endpos (E_call (with_loc $startpos($2) $endpos($2) "OR", [e1; e2])) }
  | e1=expr EQUALS e2=expr
    { with_loc $startpos $endpos (E_call (with_loc $startpos($2) $endpos($2) "=", [e1; e2])) }
  | fn=id LPAREN args=separated_list(COMMA, expr) RPAREN
    { with_loc $startpos $endpos (E_call (fn, args)) }
  | id=id
    { with_loc $startpos $endpos (E_value id) }
