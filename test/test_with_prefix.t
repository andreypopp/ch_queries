all syntax forms can be used with `%ch.*` prefix as well, we test just the `%ch.q` here:
  $ ./compile_and_run '
  > let users = [%ch.q "SELECT users.x AS x FROM public.users WHERE users.is_active"];;
  > #show users
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:
        (Ch_queries.map_from_scope
           (Ch_queries.from
              (Ch_database.Public.users ~alias:"users" ~final:false))
           (fun (users : _ Ch_queries.scope) ->
             let __q =
               object
                 method users = users
               end
             in
             object
               method users = users
               method x = __q#users#query ?alias:(Some "x") (fun __q -> __q#x)
             end))
      ~select:(fun __q ->
        object
          method x = __q#x
        end)
      ~where:(fun __q ->
        __q#users#query ?alias:(Some "is_active") (fun __q -> __q#is_active))
  >>> RUNNING
  val users :
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select
