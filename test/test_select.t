there's a form `%ch.select` which generates SQL + row parser out of a query:
  $ ./compile_and_run '
  > let%ch.select users = {|
  >   SELECT
  >     users.id::Int32 AS id,
  >     users.is_active::Bool AS is_active 
  >   FROM $users::public.users|};;
  > #show users_row
  > #show users
  > let sql, _ = users ~users:(Ch_database.Public.users ~final:false) ();;
  > print_endline sql;;
  > ' --run-only
  >>> RUNNING
  type users_row = { id : int; is_active : bool; }
  val users :
    users:(alias:string ->
           Ch_database.Public.users Ch_queries.scope Ch_queries.from_one) ->
    unit -> string * (Ch_queries.json list -> users_row)
  SELECT q._2, q._1
  FROM (
    SELECT users.is_active AS _1, users.id AS _2 FROM public.users AS users) AS q

It's possible to use `Any` type to skip parsing a certain field (it'll be returned as JSON instead):
  $ ./compile_and_run '
  > let%ch.select users = {|
  >   SELECT
  >     users.id::Int32 AS id,
  >     users.is_active::Any AS is_active 
  >   FROM $users::public.users|};;
  > #show users_row
  > '
  >>> PREPROCESSING
  include struct
    type nonrec users_row = { id : int; is_active : Ch_queries.json }
  
    let users ~users () =
      Ch_queries.query
        (Ch_queries.select ()
           ~from:
             (Ch_queries.map_from_scope
                (Ch_queries.from
                   (users ~alias:"users"
                     : Ch_database.Public.users Ch_queries.scope
                       Ch_queries.from_one))
                (fun (users : _ Ch_queries.scope) ->
                  let __q =
                    object
                      method users = users
                    end
                  in
                  object
                    method users = users
  
                    method id =
                      (__q#users#query (fun __q -> __q#id)
                        : ( Ch_queries.non_null,
                            int Ch_queries.number )
                          Ch_queries.expr)
  
                    method is_active =
                      (__q#users#query (fun __q -> __q#is_active)
                        : (_, _) Ch_queries.expr)
                  end))
           ~select:(fun __q ->
             object
               method id = __q#id
               method is_active = __q#is_active
             end))
        (fun (__q : < q : _ Ch_queries.scope >) ->
          Ch_queries.Row.( let+ )
            (Ch_queries.Row.( and+ )
               (Ch_queries.Row.col
                  (__q#q#query (fun __q -> __q#id))
                  Ch_queries.Row.int)
               (Ch_queries.Row.col
                  (__q#q#query (fun __q -> __q#is_active))
                  Ch_queries.Row.any))
            (fun (id, is_active) -> { id; is_active }))
  end
  >>> RUNNING
  type users_row = { id : int; is_active : Ch_queries.json; }
