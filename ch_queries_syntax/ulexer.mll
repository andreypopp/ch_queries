{
  open Uparser

  exception Error of string

  let string_of_token : Uparser.token -> string = function
    | Uparser.PARAM s -> Printf.sprintf "PARAM(%s)" s
    | Uparser.COLUMN (x, y) -> Printf.sprintf "COLUMN(%s %s)" x y
    | Uparser.SQL s -> Printf.sprintf "SQL(%s)" s
    | Uparser.EOF -> "EOF"

  let rollback_match lexbuf ~matched =
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - String.length matched
}

let whitespace = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let id_rest = letter | digit | '_'
let id = (letter | '_') id_rest*
let param = '$'

rule token = parse
  | whitespace+              { token lexbuf }
  | newline                  { Lexing.new_line lexbuf; token lexbuf }
  | param (id as param)      { PARAM param }
  | (id as x) "." (id as y)  { COLUMN (x, y) }
  | eof                      { EOF }
  | _ as c                   {
    let lex_start_p = Lexing.lexeme_start_p lexbuf in
    let buf = Buffer.create 32 in
    Buffer.add_char buf c;
    let sql = sql_fragment buf lexbuf in
    lexbuf.Lexing.lex_start_p <- lex_start_p; (* so the start location for this token is correct *)
    SQL sql }

and sql_fragment buf = parse
  | param id as matched        { rollback_match lexbuf ~matched; Buffer.contents buf }
  | id "." id as matched       { rollback_match lexbuf ~matched; Buffer.contents buf }
  | eof                        { Buffer.contents buf }
  | newline                    { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; sql_fragment buf lexbuf }
  | _ as c                     { Buffer.add_char buf c; sql_fragment buf lexbuf }

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
        match token with Uparser.EOF -> () | _ -> loop ()
      in
      loop ()
    with Error msg -> failwith ("Lexical error: " ^ msg)

  let token () = token
}
