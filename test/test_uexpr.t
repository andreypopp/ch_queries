Test untyped expressions

  $ ./compile_and_run '
  > let e x = [%eu "someUnknownFunction(?x, interval 1 day)"];;
  > #show e
  > '
  >>> PREPROCESSING
  let e x =
    Queries.unsafe_concat
      [
        Queries.A_expr (Queries.unsafe "someUnknownFunction(");
        Queries.A_expr x;
        Queries.A_expr (Queries.unsafe ", interval 1 day)");
      ]
  >>> RUNNING
  val e : ('a, 'b) Queries.expr -> ('c, 'd) Queries.expr

  $ ./compile_and_run '
  > let e x : [%t "Int32"] = [%eu "someUnknownFunction(?x, interval 1 day)"];;
  > #show e
  > '
  >>> PREPROCESSING
  let e x : (Queries.non_null, int Queries.number) Queries.expr =
    Queries.unsafe_concat
      [
        Queries.A_expr (Queries.unsafe "someUnknownFunction(");
        Queries.A_expr x;
        Queries.A_expr (Queries.unsafe ", interval 1 day)");
      ]
  >>> RUNNING
  val e :
    ('a, 'b) Queries.expr ->
    (Queries.non_null, int Queries.number) Queries.expr

  $ ./compile_and_run '
  > let e (x : [%t "UInt64"]) : [%t "Int32"] = [%eu "someUnknownFunction(?x, interval 1 day)"];;
  > #show e
  > '
  >>> PREPROCESSING
  let e (x : (Queries.non_null, int64 Queries.number) Queries.expr) :
      (Queries.non_null, int Queries.number) Queries.expr =
    Queries.unsafe_concat
      [
        Queries.A_expr (Queries.unsafe "someUnknownFunction(");
        Queries.A_expr x;
        Queries.A_expr (Queries.unsafe ", interval 1 day)");
      ]
  >>> RUNNING
  val e :
    (Queries.non_null, int64 Queries.number) Queries.expr ->
    (Queries.non_null, int Queries.number) Queries.expr

  $ ./compile_and_run '
  > let e users : [%t "Int32"] = [%eu "someUnknownFunction(users.x, interval 1 day)"];;
  > #show e
  > '
  >>> PREPROCESSING
  let e users : (Queries.non_null, int Queries.number) Queries.expr =
    Queries.unsafe_concat
      [
        Queries.A_expr (Queries.unsafe "someUnknownFunction(");
        Queries.A_expr (users#query (fun users -> users#x));
        Queries.A_expr (Queries.unsafe ", interval 1 day)");
      ]
  >>> RUNNING
  val e :
    < query : (< x : 'a; .. > -> 'a) -> ('b, 'c) Queries.expr; .. > ->
    (Queries.non_null, int Queries.number) Queries.expr
