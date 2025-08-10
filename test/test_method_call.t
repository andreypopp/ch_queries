Method call syntax:
  $ ./compile_and_run '
  > let q table = [%e "table.compute(42, 43)"]
  > '
  >>> PREPROCESSING
  let q table =
    table#query (fun table ->
        table#compute (Ch_queries.int 42) (Ch_queries.int 43))
  >>> RUNNING

Method call in query:
  $ ./compile_and_run '
  > let q users = [%q "SELECT users.count() FROM ?users"]
  > '
  >>> PREPROCESSING
  let q users =
    Ch_queries.select ()
      ~from:(Ch_queries.from (users ~alias:"users"))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method _1 = users#query (fun users -> users#count)
        end)
  >>> RUNNING
