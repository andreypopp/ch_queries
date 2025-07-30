
basic form:
  $ ./compile_and_run '
  > let x users = [%expr "users.x"]
  > '
  >>> PREPROCESSING
  let x users = users#query (fun users -> users#x)
  >>> RUNNING

function calls of the form `FUNCTION_NAME(..)` resolve to `Queries.Expr.FUNCTION_NAME ..`:
  $ ./compile_and_run '
  > let x users = [%expr "coalesce(users.x, 1)"]
  > '
  >>> PREPROCESSING
  let x users =
    Queries.Expr.coalesce (users#query (fun users -> users#x)) (Queries.int 1)
  >>> RUNNING

AND/OR operators:
  $ ./compile_and_run '
  > let x users = [%expr "users.is_active OR users.is_deleted AND users.x"]
  > '
  >>> PREPROCESSING
  let x users =
    Queries.Expr.( || )
      (users#query (fun users -> users#is_active))
      (Queries.Expr.( && )
         (users#query (fun users -> users#is_deleted))
         (users#query (fun users -> users#x)))
  >>> RUNNING

arrays:
  $ ./compile_and_run '
  > let x = [%expr "[1,2,3]"]
  > '
  >>> PREPROCESSING
  let x = Queries.array [ Queries.int 1; Queries.int 2; Queries.int 3 ]
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%expr "[]"]
  > '
  >>> PREPROCESSING
  let x = Queries.array []
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%expr "[toNullable(1)]"]
  > '
  >>> PREPROCESSING
  let x = Queries.array [ Queries.Expr.toNullable (Queries.int 1) ]
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%expr "[1, true]"]
  > ' 2>&1 | rg -v File
  >>> PREPROCESSING
  let x = Queries.array [ Queries.int 1; Queries.bool true ]
  >>> RUNNING
  Error: This expression has type (Queries.non_null, bool) Queries.expr
         but an expression was expected of type
           (Queries.non_null, int Queries.number) Queries.expr
         Type bool is not compatible with type int Queries.number
