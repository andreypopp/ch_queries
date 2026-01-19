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
  SELECT users.id AS id, users.is_active AS is_active FROM public.users AS users

It's possible to use `Any` type to skip parsing a certain field (it'll be returned as JSON instead):
  $ ./compile_and_run '
  > let%ch.select users = {|
  >   SELECT
  >     users.id::Int32 AS id,
  >     users.is_active::Any AS is_active 
  >   FROM $users::public.users|};;
  > #show users_row
  > ' --run-only
  >>> RUNNING
  type users_row = { id : int; is_active : Ch_queries.json; }

It's possible to use `Custom(<ocamltype>)` type to specify custom type for a
certain field, the `<ocamltype>_of_json` function must be defined to parse JSON
into that type:
  $ ./compile_and_run '
  > type x = X of string
  > let x_of_json = function
  >   | `String s -> X s
  >   | _ -> failwith "invalid X, expected string"
  > let%ch.select users = {|
  >   SELECT
  >     users.id::Int32 AS id,
  >     users.x::Custom(x) AS is_active 
  >   FROM public.users|};;
  > #show users_row
  > ' --run-only
  >>> RUNNING
  type users_row = { id : int; is_active : x; }

It's possible to use an existing record type with @ch.row attribute:
  $ ./compile_and_run '
  > type user = {
  >   id: int;
  >   is_active: bool;
  > }
  > let%ch.select[@ch.row user] users = {|
  >   SELECT
  >     users.id::Int32 AS id,
  >     users.is_active::Bool AS is_active
  >   FROM $users::public.users|};;
  > #show users
  > ' --run-only
  >>> RUNNING
  val users :
    users:(alias:string ->
           Ch_database.Public.users Ch_queries.scope Ch_queries.from_one) ->
    unit -> string * (Ch_queries.json list -> user)
