propagating scopes
  $ ./compile_and_run '
  > let x {%s|table ...|} = {%e|table.col|};;
  > let y __q = {%e|$.x + 1|};;
  > #show y;;
  > '
  >>> PREPROCESSING
  let x (__q : < table : < .. > Ch_queries.scope >) =
    __q#table#query ?alias:(Some "col") (fun __q -> __q#col)
  
  let y __q = Ch_queries.Expr.( + ) (x __q) (Ch_queries.int 1)
  >>> RUNNING
  val y :
    < table : < col : (Ch_queries.non_null, int Ch_queries.number)
                      Ch_queries.expr;
                .. >
              Ch_queries.scope > ->
    (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
