simple OCaml expression:
  $ ./compile_and_run '
  > let x = 42
  > let result users = [%expr "?{x + 1}"]
  > '
  >>> PREPROCESSING
  let x = 42
  let result users = x + 1
  >>> RUNNING

OCaml expression with function call:
  $ ./compile_and_run '
  > let name = "alice"
  > let result users = [%expr "?{String.uppercase_ascii name}"]
  > '
  >>> PREPROCESSING
  let name = "alice"
  let result users = String.uppercase_ascii name
  >>> RUNNING

OCaml expression in query:
  $ ./compile_and_run '
  > let x = 42
  > let q users = [%query "SELECT ?{x + 1} AS x FROM ?users"]
  > '
  >>> PREPROCESSING
  let x = 42
  
  let q users =
    Queries.select ()
      ~from:(Queries.from (users ~alias:"users"))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method x = x + 1
        end)
  >>> RUNNING
