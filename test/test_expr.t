
basic form:
  $ ./compile_and_run '
  > let x __q = [%e "users.x"]
  > '
  >>> PREPROCESSING
  let x __q = __q#users#query (fun users -> users#x)
  >>> RUNNING

function calls of the form `FUNCTION_NAME(..)` resolve to `Ch_queries.Expr.FUNCTION_NAME ..`:
  $ ./compile_and_run '
  > let x __q = [%e "coalesce(users.x, 1)"]
  > '
  >>> PREPROCESSING
  let x __q =
    Ch_queries.Expr.coalesce
      (__q#users#query (fun users -> users#x))
      (Ch_queries.int 1)
  >>> RUNNING

AND/OR operators:
  $ ./compile_and_run '
  > let x __q = [%e "users.is_active OR users.is_deleted AND users.x"]
  > '
  >>> PREPROCESSING
  let x __q =
    Ch_queries.Expr.( || )
      (__q#users#query (fun users -> users#is_active))
      (Ch_queries.Expr.( && )
         (__q#users#query (fun users -> users#is_deleted))
         (__q#users#query (fun users -> users#x)))
  >>> RUNNING

arrays:
  $ ./compile_and_run '
  > let x = [%e "[1,2,3]"]
  > '
  >>> PREPROCESSING
  let x =
    Ch_queries.array [ Ch_queries.int 1; Ch_queries.int 2; Ch_queries.int 3 ]
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "[]"]
  > '
  >>> PREPROCESSING
  let x = Ch_queries.array []
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "[toNullable(1)]"]
  > '
  >>> PREPROCESSING
  let x = Ch_queries.array [ Ch_queries.Expr.toNullable (Ch_queries.int 1) ]
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "[1, true]"]
  > ' 2>&1 | grep -v File
  >>> PREPROCESSING
  let x = Ch_queries.array [ Ch_queries.int 1; Ch_queries.bool true ]
  >>> RUNNING
  Error: This expression has type (Ch_queries.non_null, bool) Ch_queries.expr
         but an expression was expected of type
           (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
         Type bool is not compatible with type int Ch_queries.number

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
  let x ~param = (param : (Ch_queries.non_null, string) Ch_queries.expr)
  >>> RUNNING

parameter expressions with numeric types:
  $ ./compile_and_run '
  > let x ~param = [%e "?param:Int32"]
  > '
  >>> PREPROCESSING
  let x ~param =
    (param : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr)
  >>> RUNNING

  $ ./compile_and_run '
  > let x ~param = [%e "?param:Int64"]
  > '
  >>> PREPROCESSING
  let x ~param =
    (param : (Ch_queries.non_null, int64 Ch_queries.number) Ch_queries.expr)
  >>> RUNNING

  $ ./compile_and_run '
  > let x ~param = [%e "?param:Float64"]
  > '
  >>> PREPROCESSING
  let x ~param =
    (param : (Ch_queries.non_null, float Ch_queries.number) Ch_queries.expr)
  >>> RUNNING

parameter expressions with nullable types:
  $ ./compile_and_run '
  > let x ~param = [%e "?param:Nullable(String)"]
  > '
  >>> PREPROCESSING
  let x ~param = (param : (Ch_queries.null, string) Ch_queries.expr)
  >>> RUNNING

  $ ./compile_and_run '
  > let x ~param = [%e "?param:Nullable(Int32)"]
  > '
  >>> PREPROCESSING
  let x ~param =
    (param : (Ch_queries.null, int Ch_queries.number) Ch_queries.expr)
  >>> RUNNING

parameter expressions with array types:
  $ ./compile_and_run '
  > let x ~param = [%e "?param:Array(String)"]
  > '
  >>> PREPROCESSING
  let x ~param =
    (param
      : ( Ch_queries.non_null,
          (Ch_queries.non_null, string) Ch_queries.array )
        Ch_queries.expr)
  >>> RUNNING

  $ ./compile_and_run '
  > let x ~param = [%e "?param:Array(Int32)"]
  > '
  >>> PREPROCESSING
  let x ~param =
    (param
      : ( Ch_queries.non_null,
          (Ch_queries.non_null, int Ch_queries.number) Ch_queries.array )
        Ch_queries.expr)
  >>> RUNNING

parameter expressions with nested complex types:
  $ ./compile_and_run '
  > let x ~param = [%e "?param:Array(Nullable(String))"]
  > '
  >>> PREPROCESSING
  let x ~param =
    (param
      : ( Ch_queries.non_null,
          (Ch_queries.null, string) Ch_queries.array )
        Ch_queries.expr)
  >>> RUNNING

arithmetics:
  $ ./compile_and_run '
  > let x = {%e|1+2/4*2-1|}
  > '
  >>> PREPROCESSING
  let x =
    Ch_queries.Expr.( - )
      (Ch_queries.Expr.( + ) (Ch_queries.int 1)
         (Ch_queries.Expr.( * )
            (Ch_queries.Expr.( / ) (Ch_queries.int 2) (Ch_queries.int 4))
            (Ch_queries.int 2)))
      (Ch_queries.int 1)
  >>> RUNNING

comparison:
  $ ./compile_and_run '
  > let x = {%e|1>2 and 2<3 and 5>=4 and 8<=5|}
  > '
  >>> PREPROCESSING
  let x =
    Ch_queries.Expr.( && )
      (Ch_queries.Expr.( && )
         (Ch_queries.Expr.( && )
            (Ch_queries.Expr.( > ) (Ch_queries.int 1) (Ch_queries.int 2))
            (Ch_queries.Expr.( < ) (Ch_queries.int 2) (Ch_queries.int 3)))
         (Ch_queries.Expr.( >= ) (Ch_queries.int 5) (Ch_queries.int 4)))
      (Ch_queries.Expr.( <= ) (Ch_queries.int 8) (Ch_queries.int 5))
  >>> RUNNING
