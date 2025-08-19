Test untyped expressions

  $ ./compile_and_run '
  > let e x = [%eu "someUnknownFunction(?x, interval 1 day)"];;
  > #show e
  > '
  >>> PREPROCESSING
  let e x =
    Ch_queries.unsafe_concat
      [
        Ch_queries.A_expr (Ch_queries.unsafe "someUnknownFunction(");
        Ch_queries.A_expr x;
        Ch_queries.A_expr (Ch_queries.unsafe ", interval 1 day)");
      ]
  >>> RUNNING
  val e : ('a, 'b) Ch_queries.expr -> ('c, 'd) Ch_queries.expr

  $ ./compile_and_run '
  > let e x : [%t "Int32"] = [%eu "someUnknownFunction(?x, interval 1 day)"];;
  > #show e
  > '
  >>> PREPROCESSING
  let e x : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr =
    Ch_queries.unsafe_concat
      [
        Ch_queries.A_expr (Ch_queries.unsafe "someUnknownFunction(");
        Ch_queries.A_expr x;
        Ch_queries.A_expr (Ch_queries.unsafe ", interval 1 day)");
      ]
  >>> RUNNING
  val e :
    ('a, 'b) Ch_queries.expr ->
    (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr

  $ ./compile_and_run '
  > let e (x : [%t "UInt64"]) : [%t "Int32"] = [%eu "someUnknownFunction(?x, interval 1 day)"];;
  > #show e
  > '
  >>> PREPROCESSING
  let e (x : (Ch_queries.non_null, int64 Ch_queries.number) Ch_queries.expr) :
      (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr =
    Ch_queries.unsafe_concat
      [
        Ch_queries.A_expr (Ch_queries.unsafe "someUnknownFunction(");
        Ch_queries.A_expr x;
        Ch_queries.A_expr (Ch_queries.unsafe ", interval 1 day)");
      ]
  >>> RUNNING
  val e :
    (Ch_queries.non_null, int64 Ch_queries.number) Ch_queries.expr ->
    (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr

  $ ./compile_and_run '
  > let e __q : [%t "Int32"] = [%eu "someUnknownFunction(users.x, interval 1 day)"];;
  > #show e
  > '
  >>> PREPROCESSING
  let e __q : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr =
    Ch_queries.unsafe_concat
      [
        Ch_queries.A_expr (Ch_queries.unsafe "someUnknownFunction(");
        Ch_queries.A_expr (__q#users#query (fun __q -> __q#x));
        Ch_queries.A_expr (Ch_queries.unsafe ", interval 1 day)");
      ]
  >>> RUNNING
  val e :
    < users : < query : (< x : 'a; .. > -> 'a) -> ('b, 'c) Ch_queries.expr;
                .. >;
      .. > ->
    (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
