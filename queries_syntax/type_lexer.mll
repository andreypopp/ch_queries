{
  open Type_parser

  exception Error of string

  let string_of_token : Type_parser.token -> string = function
    | Type_parser.ID s -> Printf.sprintf "ID(%s)" s
    | Type_parser.LPAREN -> "LPAREN"
    | Type_parser.RPAREN -> "RPAREN"
    | Type_parser.COMMA -> "COMMA"
    | Type_parser.EOF -> "EOF"

}

let whitespace = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let id_char = letter | digit | '_'
let id = (letter | '_') id_char*

rule token = parse
  | whitespace+         { token lexbuf }
  | newline             { Lexing.new_line lexbuf; token lexbuf }
  | id as s             { ID s }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | ','                 { COMMA }
  | eof                 { EOF }
  | _ as c              { raise (Error ("Unexpected character: " ^ String.make 1 c)) }

{
  let tokenize_debug s =
    let string_of_pos start_pos end_pos =
      Printf.sprintf "line %d, col %d-%d" start_pos.Lexing.pos_lnum
        (start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol + 1)
        (end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol + 1)
    in
    let lexbuf = Lexing.from_string s in
    try
      let rec loop () =
        let token = token lexbuf in
        let start_pos = Lexing.lexeme_start_p lexbuf in
        let end_pos = Lexing.lexeme_end_p lexbuf in
        print_endline (Printf.sprintf "%s %s" (string_of_token token) (string_of_pos start_pos end_pos));
        match token with Type_parser.EOF -> () | _ -> loop ()
      in
      loop ()
    with Error msg -> failwith ("Lexical error: " ^ msg)
}