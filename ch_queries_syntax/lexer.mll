{
  open Parser

  exception Error of string

  let errorf fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

  let keywords = [
    ("SELECT", SELECT);
    ("FROM", FROM);
    ("PREWHERE", PREWHERE);
    ("WHERE", WHERE);
    ("QUALIFY", QUALIFY);
    ("HAVING", HAVING);
    ("AS", AS);
    ("TRUE", TRUE);
    ("FALSE", FALSE);
    ("AND", AND);
    ("OR", OR);
    ("LIKE", LIKE);
    ("NOT", NOT);
    ("JOIN", JOIN);
    ("INNER", INNER);
    ("LEFT", LEFT);
    ("OPTIONAL", OPTIONAL);
    ("ON", ON);
    ("GROUP", GROUP);
    ("BY", BY);
    ("ORDER", ORDER);
    ("ASC", ASC);
    ("DESC", DESC);
    ("OVER", OVER);
    ("PARTITION", PARTITION);
    ("LIMIT", LIMIT);
    ("OFFSET", OFFSET);
    ("CLUSTER", CLUSTER);
    ("VIEW", VIEW);
    ("FINAL", FINAL);
    ("IN", IN);
    ("UNION", UNION);
    ("SETTINGS", SETTINGS);
    ("INTERVAL", INTERVAL);
    ("YEAR", YEAR);
    ("YEARS", YEAR);
    ("MONTH", MONTH);
    ("MONTHS", MONTH);
    ("WEEK", WEEK);
    ("WEEKS", WEEK);
    ("DAY", DAY);
    ("DAYS", DAY);
    ("HOUR", HOUR);
    ("HOURS", HOUR);
    ("MINUTE", MINUTE);
    ("MINUTES", MINUTE);
    ("SECOND", SECOND);
    ("SECONDS", SECOND);
    ("WITH", WITH);
    ("FILL", FILL);
    ("STEP", STEP);
    ("TO", TO);
    ("INTERPOLATE", INTERPOLATE);
  ]

  let keyword_table = Hashtbl.create 16

  let () =
    List.iter keywords ~f:(fun (kwd, tok) ->
      Hashtbl.add keyword_table kwd tok
    )

  let get_keyword_or_id s =
    try Hashtbl.find keyword_table (String.uppercase_ascii s)
    with Not_found -> ID s

  let string_of_token : Parser.token -> string = function
    | Parser.ID s -> Printf.sprintf "ID(%s)" s
    | Parser.PARAM s -> Printf.sprintf "PARAM(%s)" s
    | Parser.PARAM_SPLICE s -> Printf.sprintf "PARAM_SPLICE(%s)" s
    | Parser.CH_PARAM s -> Printf.sprintf "CH_PARAM(%s)" s
    | Parser.OCAML_EXPR s -> Printf.sprintf "OCAML_EXPR(%s)" s
    | Parser.UNSAFE _ -> Printf.sprintf "UNSAFE(...)"
    | Parser.STRING s -> Printf.sprintf "STRING(%s)" s
    | Parser.NUMBER n -> Printf.sprintf "NUMBER(%d)" n
    | Parser.FLOAT n -> Printf.sprintf "FLOAT(%g)" n
    | Parser.TRUE -> "TRUE"
    | Parser.FALSE -> "FALSE"
    | Parser.SELECT -> "SELECT"
    | Parser.FROM -> "FROM"
    | Parser.PREWHERE -> "PREWHERE"
    | Parser.WHERE -> "WHERE"
    | Parser.AS -> "AS"
    | Parser.DOT -> "DOT"
    | Parser.COLONCOLON -> "COLONCOLON"
    | Parser.LPAREN -> "LPAREN"
    | Parser.RPAREN -> "RPAREN"
    | Parser.LBRACKET -> "LBRACKET"
    | Parser.RBRACKET -> "RBRACKET"
    | Parser.COMMA -> "COMMA"
    | Parser.PLUS -> "PLUS"
    | Parser.MINUS -> "MINUS"
    | Parser.STAR -> "STAR"
    | Parser.SLASH -> "SLASH"
    | Parser.EQUALS -> "EQUALS"
    | Parser.GT -> "GT"
    | Parser.LT -> "LT"
    | Parser.GE -> "GE"
    | Parser.LE -> "LE"
    | Parser.NOT_EQUAL -> "NOT_EQUAL"
    | Parser.AND -> "AND"
    | Parser.OR -> "OR"
    | Parser.LIKE -> "LIKE"
    | Parser.NOT -> "NOT"
    | Parser.INNER -> "INNER"
    | Parser.JOIN -> "JOIN"
    | Parser.LEFT -> "LEFT"
    | Parser.OPTIONAL -> "OPTIONAL"
    | Parser.ON -> "ON"
    | Parser.GROUP -> "GROUP"
    | Parser.BY -> "BY"
    | Parser.HAVING -> "HAVING"
    | Parser.ORDER -> "ORDER"
    | Parser.ASC -> "ASC"
    | Parser.DESC -> "DESC"
    | Parser.OVER -> "OVER"
    | Parser.PARTITION -> "PARTITION"
    | Parser.QUALIFY -> "QUALIFY"
    | Parser.LIMIT -> "LIMIT"
    | Parser.OFFSET -> "OFFSET"
    | Parser.CLUSTER -> "CLUSTER"
    | Parser.VIEW -> "VIEW"
    | Parser.FINAL -> "FINAL"
    | Parser.IN -> "IN"
    | Parser.UNION -> "UNION"
    | Parser.SETTINGS -> "SETTINGS"
    | Parser.INTERVAL -> "INTERVAL"
    | Parser.YEAR -> "YEAR"
    | Parser.MONTH -> "MONTH"
    | Parser.WEEK -> "WEEK"
    | Parser.DAY -> "DAY"
    | Parser.HOUR -> "HOUR"
    | Parser.MINUTE -> "MINUTE"
    | Parser.SECOND -> "SECOND"
    | Parser.ARROW -> "ARROW"
    | Parser.WITH -> "WITH"
    | Parser.FILL -> "FILL"
    | Parser.STEP -> "STEP"
    | Parser.TO -> "TO"
    | Parser.INTERPOLATE -> "INTERPOLATE"
    | Parser.AS_MATERIALIZED -> "AS_MATERIALIZED"
    | Parser.AS_LPAREN -> "AS_LPAREN"
    | Parser.AS_PARAM s -> Printf.sprintf "AS_PARAM(%s)" s
    | Parser.QUESTION -> "QUESTION"
    | Parser.DOT_DOT_DOT -> "DOT_DOT_DOT"
    | Parser.EOF -> "EOF"

}

let whitespace = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let id_char = letter | digit | '_'
let id = (letter | '_') id_char*
let number = digit+
let float = digit+ '.' digit* | '.' digit+
let param_char = '$'

rule token = parse
  | whitespace+         { token lexbuf }
  | newline             { Lexing.new_line lexbuf; token lexbuf }
  | float as s          { FLOAT (float_of_string s) }
  | number as s         { NUMBER (int_of_string s) }
  | '\''                {
    (* remember start position to restore later, needed because string token is matched through multiple patterns *)
    let lex_start_p = Lexing.lexeme_start_p lexbuf in
    let s = string_literal (Buffer.create 16) lexbuf in
    lexbuf.Lexing.lex_start_p <- lex_start_p;
    STRING s }
  | param_char (id as s) '.' '.' '.' { PARAM_SPLICE s }
  | param_char '{'             {
    (* remember start position to restore later *)
    let lex_start_p = Lexing.lexeme_start_p lexbuf in
    let s = ocaml_expr (Buffer.create 32) 0 lexbuf in
    lexbuf.Lexing.lex_start_p <- lex_start_p;
    OCAML_EXPR s }
  | "unsafe" whitespace* '{' {
    (* remember start position to restore later *)
    let lex_start_p = Lexing.lexeme_start_p lexbuf in
    let s = unsafe_expr (Buffer.create 32) 0 lexbuf in
    lexbuf.Lexing.lex_start_p <- lex_start_p;
    UNSAFE s }
  | '{'                 {
    (* remember start position to restore later *)
    let lex_start_p = Lexing.lexeme_start_p lexbuf in
    let buf = Buffer.create 32 in
    Buffer.add_char buf '{';
    let s = ch_param buf 0 lexbuf in
    lexbuf.Lexing.lex_start_p <- lex_start_p;
    CH_PARAM s }
  | param_char (id as s) { PARAM s }
  | id as s             { get_keyword_or_id s }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | '['                 { LBRACKET }
  | ']'                 { RBRACKET }
  | ','                 { COMMA }
  | '.'                 { DOT }
  | "..."               { DOT_DOT_DOT }
  | ':' ':'             { COLONCOLON }
  | '?'                 { QUESTION }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { STAR }
  | '/'                 { SLASH }
  | '='                 { EQUALS }
  | '>'                 { GT }
  | '<'                 { LT }
  | ">="                { GE }
  | "<="                { LE }
  | "!="                { NOT_EQUAL }
  | "<>"                { NOT_EQUAL }
  | "->"                { ARROW }
  | eof                 { EOF }
  | _ as c              { errorf "Unexpected character: %c" c }

and string_literal buf = parse
  | '\'' '\''           { Buffer.add_char buf '\''; string_literal buf lexbuf }
  | '\\' '\\'           { Buffer.add_char buf '\\'; string_literal buf lexbuf }
  | '\\' '\''           { Buffer.add_char buf '\''; string_literal buf lexbuf }
  | '\\' 'n'            { Buffer.add_char buf '\n'; string_literal buf lexbuf }
  | '\\' 't'            { Buffer.add_char buf '\t'; string_literal buf lexbuf }
  | '\\' 'r'            { Buffer.add_char buf '\r'; string_literal buf lexbuf }
  | '\\' (_ as c)       { Buffer.add_char buf '\\'; Buffer.add_char buf c; string_literal buf lexbuf }
  | '\''                { Buffer.contents buf }
  | newline             { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; string_literal buf lexbuf }
  | _ as c              { Buffer.add_char buf c; string_literal buf lexbuf }
  | eof                 { errorf "Unterminated string literal" }

and ocaml_expr buf brace_count = parse
  | '{'                 { Buffer.add_char buf '{'; ocaml_expr buf (brace_count + 1) lexbuf }
  | '}'                 {
    if brace_count = 0 then
      Buffer.contents buf
    else (
      Buffer.add_char buf '}';
      ocaml_expr buf (brace_count - 1) lexbuf
    ) }
  | newline             { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; ocaml_expr buf brace_count lexbuf }
  | _ as c              { Buffer.add_char buf c; ocaml_expr buf brace_count lexbuf }
  | eof                 { errorf "Unterminated OCaml expression: missing closing '}'" }

and ch_param buf brace_count = parse
  | '{'                 { Buffer.add_char buf '{'; ch_param buf (brace_count + 1) lexbuf }
  | '}'                 {
    Buffer.add_char buf '}';
    if brace_count = 0 then
      Buffer.contents buf
    else
      ch_param buf (brace_count - 1) lexbuf
    }
  | newline             { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; ch_param buf brace_count lexbuf }
  | _ as c              { Buffer.add_char buf c; ch_param buf brace_count lexbuf }
  | eof                 { errorf "Unterminated param type expression: missing closing '}'" }

and unsafe_expr buf brace_count = parse
  | '{'                 { Buffer.add_char buf '{'; unsafe_expr buf (brace_count + 1) lexbuf }
  | '}'                 {
    if brace_count = 0 then (
      let content = Buffer.contents buf in
      let ulexbuf = Lexing.from_string content in
      (* Adjust the location to account for the 'unsafe {' prefix *)
      let start_pos = lexbuf.Lexing.lex_start_p in
      let adjusted_start = { start_pos with
        pos_cnum = start_pos.pos_cnum + String.length "unsafe {"
      } in
      ulexbuf.Lexing.lex_start_p <- adjusted_start;
      ulexbuf.Lexing.lex_curr_p <- adjusted_start;
      try
         Uparser.a_uexpr (Ulexer.token ()) ulexbuf
       with
       | Uparser.Error -> errorf "error parsing unsafe { %s }" content
       | Ulexer.Error msg -> errorf "error parsing unsafe { %s }: %s" content msg
    ) else (
      Buffer.add_char buf '}';
      unsafe_expr buf (brace_count - 1) lexbuf
    ) }
  | newline             { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; unsafe_expr buf brace_count lexbuf }
  | _ as c              { Buffer.add_char buf c; unsafe_expr buf brace_count lexbuf }
  | eof                 { errorf "Unterminated unsafe expression: missing closing '}'" }

{
  type lexstate = {
    mutable penging: Parser.token option;
    mutable after_as: bool;
  }

  let token () = 
    let state = {after_as=false; penging=None} in
    let rec produce lexbuf =
      match state.penging with
      | Some tok -> 
        state.penging <- None;
        tok
      | None ->
      let tok = token lexbuf in
      match state.after_as, tok with
      | true, Parser.LPAREN ->
        state.after_as <- false;
        Parser.AS_LPAREN
      | true, Parser.ID(id) when String.(equal (uppercase_ascii id) "MATERIALIZED") ->
        state.after_as <- false;
        Parser.AS_MATERIALIZED
      | true, Parser.PARAM s ->
        state.after_as <- false;
        Parser.AS_PARAM s
      | true, tok ->
        state.after_as <- false;
        state.penging <- Some tok;
        Parser.AS
      | false, Parser.AS ->
        state.after_as <- true;
        produce lexbuf
      | false, tok -> tok
    in
    produce

  let tokenize_debug q =
    let string_of_pos start_pos end_pos =
      Printf.sprintf "line %d, col %d-%d" start_pos.Lexing.pos_lnum
        (start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol + 1)
        (end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol + 1)
    in
    let lexbuf = Lexing.from_string q in
    let token = token () in
    try
      let rec loop () =
        let token = token lexbuf in
        let start_pos = Lexing.lexeme_start_p lexbuf in
        let end_pos = Lexing.lexeme_end_p lexbuf in
        print_endline (Printf.sprintf "%s %s" (string_of_token token) (string_of_pos start_pos end_pos));
        match token with Parser.EOF -> () | _ -> loop ()
      in
      loop ()
    with Error msg -> failwith ("Lexical error: " ^ msg)

}
