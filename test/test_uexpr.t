Test untyped expressions

  $ ./compile_and_run '
  > let e x = [%uexpr "someUnknownFunction(?x, interval 1 day)"];;
  > #show e
  > '
  >>> PREPROCESSING
  let e x =
    Queries.unsafe_concat
      (let open Queries.Args in
       [
         Queries.unsafe "someUnknownFunction(";
         x;
         Queries.unsafe ", interval 1 day)";
       ])
  >>> RUNNING
  val e : ('a, 'b) Queries.expr -> ('c, 'd) Queries.expr

  $ ./compile_and_run '
  > let e x : [%typ "Int32"] = [%uexpr "someUnknownFunction(?x, interval 1 day)"];;
  > #show e
  > '
  >>> PREPROCESSING
  let e x : (Queries.non_null, int Queries.number) Queries.expr =
    Queries.unsafe_concat
      (let open Queries.Args in
       [
         Queries.unsafe "someUnknownFunction(";
         x;
         Queries.unsafe ", interval 1 day)";
       ])
  >>> RUNNING
  val e :
    ('a, 'b) Queries.expr ->
    (Queries.non_null, int Queries.number) Queries.expr

  $ ./compile_and_run '
  > let e (x : [%typ "UInt64"]) : [%typ "Int32"] = [%uexpr "someUnknownFunction(?x, interval 1 day)"];;
  > #show e
  > '
  >>> PREPROCESSING
  let e (x : (Queries.non_null, int64 Queries.number) Queries.expr) :
      (Queries.non_null, int Queries.number) Queries.expr =
    Queries.unsafe_concat
      (let open Queries.Args in
       [
         Queries.unsafe "someUnknownFunction(";
         x;
         Queries.unsafe ", interval 1 day)";
       ])
  >>> RUNNING
  val e :
    (Queries.non_null, int64 Queries.number) Queries.expr ->
    (Queries.non_null, int Queries.number) Queries.expr
