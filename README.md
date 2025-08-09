# queries

A library to generate ClickHouse SQL queries in a typesafe way with OCaml.

## queries.ppx

The main mode of using queries is through the `queries.ppx` preprocessor. It
allows you to write queries in SQL-like syntax:
```ocaml
let users = {%q|
    SELECT users.id, users.name
    FROM db.users
|}
```

One can also build queries by splicing OCaml values into the query, such values
are function which accept query scope as argument:
```ocaml
let users ~where = {%q|
    SELECT users.id AS id, users.name AS name
    FROM public.users
    WHERE users.is_active AND ?where
|}
```

## `%e` - expressions

There's also expressions syntax, so one can define expressions that can be
spliced into a query later:
```ocaml
let ok users = {%e|users.is_active|}

let using_functions users = {%e|farmHash(users.name)|}
```

## `%eu` - expressions, unsafely

Sometimes you need to construct an expression using syntax which is not
supported by queries.ppx. In this case you can use the `%eu` syntax:
```ocaml
let expr = {%eu|users.name || ' ' || users.surname|}
```

Such syntax recognizes only `q.name` and `?param` constructs and passes the rest
as-is.

For now that's all, see `tests/` directory for more features and examples.
