
basic form:
  $ ./compile_and_run '
  > let x users = [%e "users.x"]
  > '
  >>> PREPROCESSING
  let x users = users#query (fun users -> users#x)
  >>> RUNNING

function calls of the form `FUNCTION_NAME(..)` resolve to `Queries.Expr.FUNCTION_NAME ..`:
  $ ./compile_and_run '
  > let x users = [%e "coalesce(users.x, 1)"]
  > '
  >>> PREPROCESSING
  let x users =
    Queries.Expr.coalesce (users#query (fun users -> users#x)) (Queries.int 1)
  >>> RUNNING

AND/OR operators:
  $ ./compile_and_run '
  > let x users = [%e "users.is_active OR users.is_deleted AND users.x"]
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
  > let x = [%e "[1,2,3]"]
  > '
  >>> PREPROCESSING
  let x = Queries.array [ Queries.int 1; Queries.int 2; Queries.int 3 ]
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "[]"]
  > '
  >>> PREPROCESSING
  let x = Queries.array []
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "[toNullable(1)]"]
  > '
  >>> PREPROCESSING
  let x = Queries.array [ Queries.Expr.toNullable (Queries.int 1) ]
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "[1, true]"]
  > ' 2>&1 | rg -v File
  >>> PREPROCESSING
  let x = Queries.array [ Queries.int 1; Queries.bool true ]
  >>> RUNNING
  Error: This expression has type (Queries.non_null, bool) Queries.expr
         but an expression was expected of type
           (Queries.non_null, int Queries.number) Queries.expr
         Type bool is not compatible with type int Queries.number

parameter expressions:
  $ ./compile_and_run '
  > let x ~param = [%e "?param"]
  > '
  >>> PREPROCESSING
  let x ~param = param
  >>> RUNNING

parameter expressions with type annotation:
  $ ./compile_and_run '
  > let x ~param = [%e "?param:String"]
  > '
  >>> PREPROCESSING
  let x ~param = (param : (Queries.non_null, string) Queries.expr)
  >>> RUNNING

parameter expressions with numeric types:
  $ ./compile_and_run '
  > let x ~param = [%e "?param:Int32"]
  > '
  >>> PREPROCESSING
  let x ~param = (param : (Queries.non_null, int Queries.number) Queries.expr)
  >>> RUNNING

  $ ./compile_and_run '
  > let x ~param = [%e "?param:Int64"]
  > '
  >>> PREPROCESSING
  let x ~param = (param : (Queries.non_null, int64 Queries.number) Queries.expr)
  >>> RUNNING

  $ ./compile_and_run '
  > let x ~param = [%e "?param:Float64"]
  > '
  >>> PREPROCESSING
  let x ~param = (param : (Queries.non_null, float Queries.number) Queries.expr)
  >>> RUNNING

parameter expressions with nullable types:
  $ ./compile_and_run '
  > let x ~param = [%e "?param:Nullable(String)"]
  > '
  >>> PREPROCESSING
  let x ~param = (param : (Queries.null, string) Queries.expr)
  >>> RUNNING

  $ ./compile_and_run '
  > let x ~param = [%e "?param:Nullable(Int32)"]
  > '
  >>> PREPROCESSING
  let x ~param = (param : (Queries.null, int Queries.number) Queries.expr)
  >>> RUNNING

parameter expressions with array types:
  $ ./compile_and_run '
  > let x ~param = [%e "?param:Array(String)"]
  > '
  >>> PREPROCESSING
  let x ~param =
    (param
      : (Queries.non_null, (Queries.non_null, string) Queries.array) Queries.expr)
  >>> RUNNING

  $ ./compile_and_run '
  > let x ~param = [%e "?param:Array(Int32)"]
  > '
  >>> PREPROCESSING
  let x ~param =
    (param
      : ( Queries.non_null,
          (Queries.non_null, int Queries.number) Queries.array )
        Queries.expr)
  >>> RUNNING

parameter expressions with nested complex types:
  $ ./compile_and_run '
  > let x ~param = [%e "?param:Array(Nullable(String))"]
  > '
  >>> PREPROCESSING
  let x ~param =
    (param
      : (Queries.non_null, (Queries.null, string) Queries.array) Queries.expr)
  >>> RUNNING
