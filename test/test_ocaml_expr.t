simple OCaml expression:
  $ ./compile_and_run '
  > let x = 42
  > let result users = [%e "${x + 1}"]
  > '
  >>> PREPROCESSING
  let x = 42
  let result users = x + 1
  >>> RUNNING

OCaml expression with function call:
  $ ./compile_and_run '
  > let name = "alice"
  > let result users = [%e "${String.uppercase_ascii name}"]
  > '
  >>> PREPROCESSING
  let name = "alice"
  let result users = String.uppercase_ascii name
  >>> RUNNING

OCaml expression in query:
  $ ./compile_and_run '
  > let x = 42
  > let q users = [%q "SELECT ${x + 1} AS x FROM $users"]
  > '
  >>> PREPROCESSING
  let x = 42
  
  let q users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from
              ((users : alias:string -> _ Ch_queries.from_one) ~alias:"users"))
           (fun (users : _ Ch_queries.scope) ->
             let __q =
               object
                 method users = users
               end
             in
             object
               method users = users
               method x = x + 1
             end))
      ~select:(fun __q ->
        object
          method x = __q#x
        end)
  >>> RUNNING
