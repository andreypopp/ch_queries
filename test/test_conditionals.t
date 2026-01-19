`if(cond, then, else)`:
  $ ./compile_and_run "
  > let e = {%e|if(true, 'then', 'else')|}
  > let () = print_endline (Ch_queries_syntax.Printer.print_expr (Ch_queries.expr_to_syntax e))
  > "
  >>> PREPROCESSING
  let e =
    (let open Ch_queries.Expr in
     if_)
      (Ch_queries.bool true) (Ch_queries.string "then") (Ch_queries.string "else")
  
  let () =
    print_endline
      (Ch_queries_syntax.Printer.print_expr (Ch_queries.expr_to_syntax e))
  >>> RUNNING
  if(true, 'then', 'else')

`multiIf(cond1, b1, cond2, b2, ..., else)`:
  $ ./compile_and_run "
  > let e = {%e|multiIf(true, 'then', false, 'else', 'default')|}
  > let () = print_endline (Ch_queries_syntax.Printer.print_expr (Ch_queries.expr_to_syntax e))
  > "
  >>> PREPROCESSING
  let e =
    Ch_queries.Expr.multiIf
      [
        (Ch_queries.bool true, Ch_queries.string "then");
        (Ch_queries.bool false, Ch_queries.string "else");
      ]
      ~else_:(Ch_queries.string "default")
  
  let () =
    print_endline
      (Ch_queries_syntax.Printer.print_expr (Ch_queries.expr_to_syntax e))
  >>> RUNNING
  multiIf(true, 'then', false, 'else', 'default')

Erroneous `multiIf(...)`:
  $ ./compile_and_run "
  > let e = {%e|multiIf('default')|}
  > "
  >>> PREPROCESSING
  File "-", line 2, characters 12-30:
  Error: multiIf(...) requires at least one case
  [1]
  $ ./compile_and_run "
  > let e = {%e|multiIf()|}
  > "
  >>> PREPROCESSING
  File "-", line 2, characters 12-21:
  Error: multiIf(k, v, ...): even number of arguments, missing else branch
  [1]
