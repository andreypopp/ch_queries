# TODO

## Add type inference

We can add a simple bidirectional type inference to infer types as ppx-time.
This will make queries more ergonomic as we can more flexibly type check them.

## method calls should support OCaml expressions within arguments, also named arguments

An example:
```ocaml
let%ch.select users = {|
  SELECT users.has_permission(permission: `Read_write) AS can_read_write
  FROM users
|}
```

## Support table functions, they should accept OCaml expressions as arguments

We want to be able to enrich database schemas with parametrized views which can
predefine fields along with WHERE/QUALIFY/HAVING/GROUP BY/ORDER BY clauses.

Depending on a structure of such views and their usafe we can produce either a
flat query or a subquery out of them.

This requires to extend the grammar to support table functions. The table
functions will allow to pass OCaml values (not SQL values) as arguments.
Therefore these table functions work like macro functions.

We expect the definition of such table functions will be done in OCaml code.

The usage will look like:
```ocaml
let%ch.select users = {|
  SELECT *
  FROM public.users_with_permission(permission: `Read_write) AS users
|}
