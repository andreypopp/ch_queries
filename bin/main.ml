open Cmdliner

let version_cmd =
  let doc = "Show version information" in
  let version () = print_endline "queries 0.1.0" in
  Cmd.v (Cmd.info "version" ~doc) Term.(const version $ const ())

let parse_cmd =
  let parse_query query force_parens =
    try
      let lexbuf = Lexing.from_string query in
      let query =
        Ch_queries_syntax.Parser.a_query
          (Ch_queries_syntax.Lexer.token ())
          lexbuf
      in
      let pretty_printed =
        Ch_queries_syntax.Printer.print_query ~force_parens query
      in
      print_endline pretty_printed
    with
    | Ch_queries_syntax.Lexer.Error msg ->
        Printf.eprintf "Lexical error: %s\n" msg;
        exit 1
    | Ch_queries_syntax.Parser.Error ->
        Printf.eprintf "Parse error\n";
        exit 1
  in
  Cmd.v
    (Cmd.info "parse" ~doc:"parse query and pretty-print it back")
    Term.(
      const parse_query
      $ Arg.(
          required
          & pos 0 (some string) None
          & info [] ~docv:"QUERY" ~doc:"SQL query to parse")
      $ Arg.(
          value & flag
          & info [ "print-force-parens" ]
              ~doc:"force parentheses when printing the query"))

let parse_expr_cmd =
  let doc = "Parse a SQL expression" in
  let expr =
    let doc = "SQL expression to parse" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"EXPR" ~doc)
  in
  let parse_expr expr_str =
    try
      let lexbuf = Lexing.from_string expr_str in
      let expr =
        Ch_queries_syntax.Parser.a_expr
          (Ch_queries_syntax.Lexer.token ())
          lexbuf
      in
      let pretty_printed = Ch_queries_syntax.Printer.print_expr expr in
      print_endline pretty_printed
    with
    | Ch_queries_syntax.Lexer.Error msg ->
        Printf.eprintf "Lexical error: %s\n" msg;
        exit 1
    | Ch_queries_syntax.Parser.Error ->
        Printf.eprintf "Parse error\n";
        exit 1
  in
  Cmd.v (Cmd.info "parse-expr" ~doc) Term.(const parse_expr $ expr)

let tokenize_cmd =
  let doc = "Tokenize a query and print tokens with locations" in
  let query =
    let doc = "SQL query to tokenize" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"QUERY" ~doc)
  in
  let debug_query q =
    try Ch_queries_syntax.Lexer.tokenize_debug q
    with Failure msg ->
      Printf.eprintf "%s\n" msg;
      exit 1
  in
  Cmd.v (Cmd.info "tokenize" ~doc) Term.(const debug_query $ query)

let main_cmd =
  let doc = "A SQL query parser" in
  let info = Cmd.info "queries" ~doc in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default
    [ version_cmd; parse_cmd; parse_expr_cmd; tokenize_cmd ]

let () = exit (Cmd.eval main_cmd)
