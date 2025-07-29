Method call syntax:
  $ ./compile_and_run '
  > let q table = [%expr "table.compute(42, 43)"]
  > '
  >>> PREPROCESSING
  let q table =
    table#query (fun table -> table#compute (Queries.int 42) (Queries.int 43))
  >>> RUNNING

Method call in query:
  $ ./compile_and_run '
  > let q users = [%query "SELECT users.count() FROM ?users"]
  > '
  >>> PREPROCESSING
  let q users =
    Queries.select ()
      ~from:(Queries.from (users ~alias:"users"))
      ~select:(fun (users : _ Queries.scope) ->
        object
          method _1 = users#query (fun users -> users#count)
        end)
  >>> RUNNING
