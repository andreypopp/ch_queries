# queries

A library to generate ClickHouse SQL queries in a typesafe way with OCaml.

## queries.ppx

The main mode of using queries is through the `queries.ppx` preprocessor. It
allows you to write queries in SQL-like syntax:
```ocaml
let users = {%query|
    SELECT users.id, users.name
    FROM db.users
|}
```

One can also build queries by splicing OCaml values into the query, such values
are function which accept query scope as argument:
```ocaml
let users ~where = {%query|
    SELECT users.id AS id, users.name AS name
    FROM public.users
    WHERE users.is_active AND ?where
|}
```

There's also expressions syntax, so one can define expressions that can be
spliced into a query later:
```ocaml
let ok users = {%expr|users.is_active|}

let using_functions users = {%expr|farmHash(users.name)|}
```

For now that's all, see `tests/` directory for more features and examples.
