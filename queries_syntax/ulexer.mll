{
  open Uparser

  exception Error of string

  let string_of_token : Uparser.token -> string = function
    | Uparser.PARAM s -> Printf.sprintf "PARAM(%s)" s
    | Uparser.SQL s -> Printf.sprintf "SQL(%s)" s
    | Uparser.EOF -> "EOF"
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
  | "?" (id as param)   { PARAM param }
  | eof                 { EOF }
  | _ as c              { 
    (* Collect SQL fragment starting from this character *)
    let lex_start_p = Lexing.lexeme_start_p lexbuf in
    let buf = Buffer.create 32 in
    Buffer.add_char buf c;
    let s = sql_fragment buf lexbuf in
    lexbuf.Lexing.lex_start_p <- lex_start_p;
    SQL s }

and sql_fragment buf = parse
  | "?" (id as param)
    {
    (* Put back the parameter token for next parsing *)
    let len = String.length ("?" ^ param) in
    lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - len;
    Buffer.contents buf
    }
  | eof                 { Buffer.contents buf }
  | newline             { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; sql_fragment buf lexbuf }
  | _ as c              { Buffer.add_char buf c; sql_fragment buf lexbuf }

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
}
