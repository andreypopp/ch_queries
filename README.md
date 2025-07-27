# queries

A library to generate SQL queries in a typesafe way with OCaml.

## queries.ppx

The main mode of using queries is through the `queries.ppx` preprocessor. It
allows you to write queries in SQL like syntax:
```ocaml
let%select users = "
    SELECT users.id, users.name
    FROM users
"
(* expands to: *)
let users =
    Queries.select ()
        ~select:(fun users -> object
                    method _1 = users#id
                    method _2 = users#name
                 end)
        ~from:(Db.users)
```

One can also build queries by splicing OCaml values into the query, such values
are function which accept query scope as argument:
```ocaml
let%select users = "
    SELECT users.id AS id, users.name AS name
    FROM users
    WHERE users.is_active AND @where
"
(* expands to: *)
let users =
    Queries.select ()
        ~select:(fun users -> object
                    method id = users#id
                    method name = users#name
                 end)
        ~from:(Db.users)
        ~where:(fun scope -> Expr.and_ scope#is_active (where scope))
```

There's also expressions syntax:

```ocaml
let ok users = [%expr "users.is_active"]
(* expands to: *)
let ok users = users#is_active

let using_functions users = [%expr "farmHash(users.name)"]
(* expands to: *)
let using_functions users = Queries.Prelude.farmHash users#name
```
