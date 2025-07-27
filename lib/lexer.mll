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
  ]

  let keyword_table = Hashtbl.create 16

  let () =
    List.iter keywords ~f:(fun (kwd, tok) ->
      Hashtbl.add keyword_table kwd tok
    )

  let get_keyword_or_id s =
    try Hashtbl.find keyword_table (String.uppercase_ascii s)
    with Not_found -> ID s
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
  | '\''                { STRING (string_literal (Buffer.create 16) lexbuf) }
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
