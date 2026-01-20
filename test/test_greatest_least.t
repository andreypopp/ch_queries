`greatest(...)`:
  $ ./compile_and_run "
  > let e = {%e|greatest(1, 2, 3)|}
  > let () = print_endline (Ch_queries_syntax.Printer.print_expr (Ch_queries.expr_to_syntax e))
  > "
  >>> PREPROCESSING
  let e =
    Ch_queries.Expr.greatest
      [ Ch_queries.int 1; Ch_queries.int 2; Ch_queries.int 3 ]
  
  let () =
    print_endline
      (Ch_queries_syntax.Printer.print_expr (Ch_queries.expr_to_syntax e))
  >>> RUNNING
  greatest(1, 2, 3)

`least(...)`:
  $ ./compile_and_run "
  > let e = {%e|least(1, 2, 3)|}
  > let () = print_endline (Ch_queries_syntax.Printer.print_expr (Ch_queries.expr_to_syntax e))
  > "
  >>> PREPROCESSING
  let e =
    Ch_queries.Expr.least [ Ch_queries.int 1; Ch_queries.int 2; Ch_queries.int 3 ]
  
  let () =
    print_endline
      (Ch_queries_syntax.Printer.print_expr (Ch_queries.expr_to_syntax e))
  >>> RUNNING
  least(1, 2, 3)
