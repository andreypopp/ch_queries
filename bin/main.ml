open Cmdliner

let version_cmd =
  let doc = "Show version information" in
  let version () = print_endline "queries 0.1.0" in
  Cmd.v (Cmd.info "version" ~doc) Term.(const version $ const ())

let parse_cmd =
  let doc = "Parse a SQL query" in
  let query =
    let doc = "SQL query to parse" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"QUERY" ~doc)
  in
  let parse_query query_str =
    try
      let lexbuf = Lexing.from_string query_str in
      let query =
        Queries_syntax.Parser.a_query Queries_syntax.Lexer.token lexbuf
      in
      let pretty_printed = Queries_syntax.Printer.print_query query in
      print_endline pretty_printed
    with
    | Queries_syntax.Lexer.Error msg ->
        Printf.eprintf "Lexical error: %s\n" msg;
        exit 1
    | Queries_syntax.Parser.Error ->
        Printf.eprintf "Parse error\n";
        exit 1
  in
  Cmd.v (Cmd.info "parse" ~doc) Term.(const parse_query $ query)

let tokenize_cmd =
  let doc = "Tokenize a query and print tokens with locations" in
  let query =
    let doc = "SQL query to tokenize" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"QUERY" ~doc)
  in
  let debug_query q =
    try Queries_syntax.Lexer.tokenize_debug q
    with Failure msg ->
      Printf.eprintf "%s\n" msg;
      exit 1
  in
  Cmd.v (Cmd.info "tokenize" ~doc) Term.(const debug_query $ query)

let main_cmd =
  let doc = "A SQL query parser" in
  let info = Cmd.info "queries" ~doc in
  Cmd.group info [ version_cmd; parse_cmd; tokenize_cmd ]

let () = exit (Cmd.eval main_cmd)
