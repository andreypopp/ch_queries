Method call syntax:
  $ ./compile_and_run '
  > let q __q = [%e "table.compute(42, 43)"]
  > '
  >>> PREPROCESSING
  let q __q =
    __q#table#query (fun __q ->
        __q#compute (Ch_queries.int 42) (Ch_queries.int 43))
  >>> RUNNING

Method call in query:
  $ ./compile_and_run '
  > let q users = [%q "SELECT users.count() FROM ?users"]
  > '
  >>> PREPROCESSING
  let q users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from (users ~alias:"users"))
           (fun (users : _ Ch_queries.scope) ->
             object
               method users = users
             end))
      ~select:(fun __q ->
        object
          method _1 = __q#users#query (fun __q -> __q#count)
        end)
  >>> RUNNING
