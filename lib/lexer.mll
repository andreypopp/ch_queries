{
  open Parser

  exception Lexical_error of string

  let keywords = [
    ("SELECT", SELECT);
    ("FROM", FROM);
    ("WHERE", WHERE);
    ("AS", AS);
    ("TRUE", TRUE);
    ("FALSE", FALSE);
    ("AND", AND);
    ("OR", OR);
    ("JOIN", JOIN);
    ("INNER", INNER);
    ("LEFT", LEFT);
    ("ON", ON);
    ("GROUP", GROUP);
    ("BY", BY);
    ("ORDER", ORDER);
    ("ASC", ASC);
    ("DESC", DESC);
    ("LIMIT", LIMIT);
    ("OFFSET", OFFSET);
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
    | Parser.ID_SPLICE s -> Printf.sprintf "ID_SPLICE(%s)" s
    | Parser.STRING s -> Printf.sprintf "STRING(%s)" s
    | Parser.NUMBER n -> Printf.sprintf "NUMBER(%d)" n
    | Parser.TRUE -> "TRUE"
    | Parser.FALSE -> "FALSE"
    | Parser.SELECT -> "SELECT"
    | Parser.FROM -> "FROM"
    | Parser.WHERE -> "WHERE"
    | Parser.AS -> "AS"
    | Parser.DOT -> "DOT"
    | Parser.LPAREN -> "LPAREN"
    | Parser.RPAREN -> "RPAREN"
    | Parser.COMMA -> "COMMA"
    | Parser.PLUS -> "PLUS"
    | Parser.MINUS -> "MINUS"
    | Parser.STAR -> "STAR"
    | Parser.SLASH -> "SLASH"
    | Parser.EQUALS -> "EQUALS"
    | Parser.AND -> "AND"
    | Parser.OR -> "OR"
    | Parser.INNER -> "INNER"
    | Parser.JOIN -> "JOIN"
    | Parser.LEFT -> "LEFT"
    | Parser.ON -> "ON"
    | Parser.GROUP -> "GROUP"
    | Parser.BY -> "BY"
    | Parser.ORDER -> "ORDER"
    | Parser.ASC -> "ASC"
    | Parser.DESC -> "DESC"
    | Parser.LIMIT -> "LIMIT"
    | Parser.OFFSET -> "OFFSET"
    | Parser.EOF -> "EOF"

}

let whitespace = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let id_char = letter | digit | '_'
let id = (letter | '_') id_char*
let number = digit+

rule token = parse
  | whitespace+         { token lexbuf }
  | newline             { Lexing.new_line lexbuf; token lexbuf }
  | number as s         { NUMBER (int_of_string s) }
  | '\''                { 
    (* remember start position to restore later, needed because string token is matched through multiple patterns *)
    let lex_start_p = Lexing.lexeme_start_p lexbuf in
    let s = string_literal (Buffer.create 16) lexbuf in
    lexbuf.Lexing.lex_start_p <- lex_start_p;
    STRING s }
  | id as s '.' '.' '.' { ID_SPLICE s }
  | id as s             { get_keyword_or_id s }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | ','                 { COMMA }
  | '.'                 { DOT }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { STAR }
  | '/'                 { SLASH }
  | '='                 { EQUALS }
  | eof                 { EOF }
  | _ as c              { raise (Lexical_error ("Unexpected character: " ^ String.make 1 c)) }

and string_literal buf = parse
  | '\''                { Buffer.contents buf }
  | '\\' '\\'           { Buffer.add_char buf '\\'; string_literal buf lexbuf }
  | '\\' '\''           { Buffer.add_char buf '\''; string_literal buf lexbuf }
  | '\\' 'n'            { Buffer.add_char buf '\n'; string_literal buf lexbuf }
  | '\\' 't'            { Buffer.add_char buf '\t'; string_literal buf lexbuf }
  | '\\' 'r'            { Buffer.add_char buf '\r'; string_literal buf lexbuf }
  | '\\' (_ as c)       { Buffer.add_char buf '\\'; Buffer.add_char buf c; string_literal buf lexbuf }
  | newline             { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; string_literal buf lexbuf }
  | _ as c              { Buffer.add_char buf c; string_literal buf lexbuf }
  | eof                 { raise (Lexical_error "Unterminated string literal") }

{
  let tokenize_debug q =
    let string_of_pos start_pos end_pos =
      Printf.sprintf "line %d, col %d-%d" start_pos.Lexing.pos_lnum
        (start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol + 1)
        (end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol + 1)
    in
    let lexbuf = Lexing.from_string q in
    try
      let rec loop () =
        let token = token lexbuf in
        let start_pos = Lexing.lexeme_start_p lexbuf in
        let end_pos = Lexing.lexeme_end_p lexbuf in
        print_endline (Printf.sprintf "%s %s" (string_of_token token) (string_of_pos start_pos end_pos));
        match token with Parser.EOF -> () | _ -> loop ()
      in
      loop ()
    with Lexical_error msg -> failwith ("Lexical error: " ^ msg)
}
