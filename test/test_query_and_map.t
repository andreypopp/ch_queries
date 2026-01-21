there's a form `%ch.query_and_map` which generates SQL + a map function:
  $ ./compile_and_run '
  > let sql, map = {%ch.query_and_map|
  >   SELECT
  >     users.id::Int32 AS id,
  >     users.is_active::Bool AS is_active
  >   FROM public.users|};;
  > print_endline sql;;
  > print_endline (map [`Int 42; `Bool true] (fun ~id ~is_active -> Printf.sprintf "id=%d active=%b" id is_active));;
  > ' --run-only
  >>> RUNNING
  SELECT users.id AS id, users.is_active AS is_active FROM public.users AS users
  id=42 active=true

with query parameters (table must be in scope):
  $ ./compile_and_run '
  > let users = Ch_database.Public.users ~final:false;;
  > let sql, map = {%ch.query_and_map|
  >   SELECT
  >     users.id::Int32 AS id
  >   FROM $users::public.users|};;
  > print_endline sql;;
  > ' --run-only
  >>> RUNNING
  SELECT users.id AS id FROM public.users AS users
