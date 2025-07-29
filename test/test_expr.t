
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
