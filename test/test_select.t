there's a form `%ch.select` which generates SQL + row parser out of a query:
  $ ./compile_and_run '
  > let%ch.select users = {|
  >   SELECT
  >     users.id::Int32 AS id,
  >     users.is_active::Bool AS name 
  >   FROM $users::public.users|};;
  > #show users_row
  > #show users
  > let sql, _ = users ~users:(Ch_database.Public.users ~final:false) ();;
  > print_endline sql;;
  > ' --run-only
  >>> RUNNING
  type users_row = { id : int; name : bool; }
  val users :
    users:(alias:string ->
           Ch_database.Public.users Ch_queries.scope Ch_queries.from_one) ->
    unit -> string * (Ch_queries.json list -> users_row)
  SELECT q._2, q._1
  FROM (
    SELECT users.is_active AS _1, users.id AS _2 FROM public.users AS users) AS q
