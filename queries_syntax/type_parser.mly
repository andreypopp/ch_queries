%{
  open Syntax
  
  let make_loc start_pos end_pos = 
    { Loc.start_pos; end_pos }
  
  let with_loc start_pos end_pos node =
    { Loc.node; loc = make_loc start_pos end_pos }
%}

%token <string> ID
%token LPAREN RPAREN COMMA
%token EOF

%start a_typ
%type <Syntax.typ> a_typ

%%

a_typ:
    t=typ EOF { t }

typ:
    id=id
    { with_loc $startpos $endpos (T id) }
  | id=id LPAREN args=separated_list(COMMA, typ) RPAREN
    { with_loc $startpos $endpos (T_app (id, args)) }

id:
    id=ID { with_loc $startpos $endpos id }