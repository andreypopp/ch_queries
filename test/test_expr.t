
  $ dotest() {
  > rm -f test_expr.ml
  > echo '
  > #use "topfind"
  > #require "queries"
  > #require "queries.ppx"
  > ' > test_expr.ml
  > echo '>>> PREPROCESSING'
  > echo "$1" | ppx_queries -impl -
  > echo '>>> RUNNING'
  > echo "$1" >> test_expr.ml
  > ocaml ./test_expr.ml
  > }

basic form:
  $ dotest '
  > let x users = [%expr "users.x"]
  > '
  >>> PREPROCESSING
  let x users = users#query (fun users -> users#x)
  >>> RUNNING

can also be applied to a structure item:
  $ dotest '
  > let%expr x users = "users.x"
  > '
  >>> PREPROCESSING
  let x users = users#query (fun users -> users#x)
  >>> RUNNING

function calls of the form `FUNCTION_NAME(..)` resolve to `Queries.Expr.FUNCTION_NAME ..`:
  $ dotest '
  > let x users = [%expr "coalesce(users.x, 1)"]
  > '
  >>> PREPROCESSING
  let x users =
    Queries.Expr.coalesce (users#query (fun users -> users#x))
      (Queries.Expr.int 1)
  >>> RUNNING

AND/OR operators:
  $ dotest '
  > let x users = [%expr "users.is_active OR users.is_deleted AND users.x"]
  > '
  >>> PREPROCESSING
  let x users =
    Queries.Expr.(||) (users#query (fun users -> users#is_active))
      (Queries.Expr.(&&) (users#query (fun users -> users#is_deleted))
         (users#query (fun users -> users#x)))
  >>> RUNNING
