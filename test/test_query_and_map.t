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

fields without explicit type annotation default to Any (returns raw json):
  $ ./compile_and_run '
  > let sql, map = {%ch.query_and_map|
  >   SELECT
  >     users.id AS id
  >   FROM public.users|};;
  > print_endline sql;;
  > let row = [`Int 42] in
  > print_endline (map row (fun ~id -> Printf.sprintf "id=%s" (match id with `Int n -> string_of_int n | _ -> "?")));;
  > ' --run-only
  >>> RUNNING
  SELECT users.id AS id FROM public.users AS users
  id=42

fields without alias get auto-generated names (_1, _2, ...):
  $ ./compile_and_run '
  > let sql, map = {%ch.query_and_map|
  >   SELECT
  >     1::Int32,
  >     true::Bool
  >   FROM public.users|};;
  > print_endline sql;;
  > let row = [`Int 1; `Bool true] in
  > print_endline (map row (fun ~_1 ~_2 -> Printf.sprintf "_1=%d _2=%b" _1 _2));;
  > ' --run-only
  >>> RUNNING
  SELECT 1 AS _1, true AS _2 FROM public.users AS users
  _1=1 _2=true

fields can mix explicit aliases with auto-generated ones:
  $ ./compile_and_run '
  > let sql, map = {%ch.query_and_map|
  >   SELECT
  >     users.id::Int32 AS id,
  >     1::Int32
  >   FROM public.users|};;
  > print_endline sql;;
  > let row = [`Int 42; `Int 1] in
  > print_endline (map row (fun ~id ~_2 -> Printf.sprintf "id=%d _2=%d" id _2));;
  > ' --run-only
  >>> RUNNING
  SELECT users.id AS id, 1 AS _2 FROM public.users AS users
  id=42 _2=1

column references without type annotation use the column name:
  $ ./compile_and_run '
  > let sql, map = {%ch.query_and_map|
  >   SELECT
  >     users.id
  >   FROM public.users|};;
  > print_endline sql;;
  > let row = [`Int 42] in
  > print_endline (map row (fun ~id -> Printf.sprintf "id=%s" (match id with `Int n -> string_of_int n | _ -> "?")));;
  > ' --run-only
  >>> RUNNING
  SELECT users.id AS id FROM public.users AS users
  id=42

column references with type annotation also extract alias (looks through ascription):
  $ ./compile_and_run '
  > let sql, map = {%ch.query_and_map|
  >   SELECT
  >     users.id::Int32
  >   FROM public.users|};;
  > print_endline sql;;
  > let row = [`Int 42] in
  > print_endline (map row (fun ~id -> Printf.sprintf "id=%d" id));;
  > ' --run-only
  >>> RUNNING
  SELECT users.id AS id FROM public.users AS users
  id=42
