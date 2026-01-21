# ch_queries

**WARNING: EXPERIMENTAL, DO NOT USE**

A library to generate ClickHouse SQL queries in a typesafe way with OCaml.

## ch_queries.ppx

The main mode of using ch_queries is through the `ch_queries.ppx` preprocessor.
It allows you to write queries in SQL-like syntax. The preprocessor will expand
the queries into OCaml code which uses the `Ch_queries` library which provides type
safe combinators for query generation.

```ocaml
# #require "ch_queries";;
# #require "ch_queries.ppx";;
```

The `ch_queries.ppx` preprocessor will generate code assuming database schema
is defined in OCaml code (usually you'd want this to be generated automatically
from DDL or some other definition):

```ocaml
open Ch_queries

module Ch_database = struct
  module Db = struct
    type users = <
      id : (non_null, int number) expr;
      name : (non_null, string) expr;
      is_active : (non_null, bool) expr;
    >
    let users =
      let scope ~alias =
        let col name = unsafe_col alias name in
        object
          method id : (non_null, int number) expr = col "id"
          method name : (non_null, string) expr = col "name"
          method is_active : (non_null, bool) expr = col "is_active"
        end
      in
      from_table ~db:"public" ~table:"users" scope
  end
end
```

## `%q` - queries

The `%q` syntax form is used to define queries:
```ocaml
# let users = {%q|SELECT users.id, users.name FROM db.users|};;
val users :
  < id : (non_null, int number) expr; name : (non_null, string) expr > scope
  select = <abstr>
```

> [!IMPORTANT]
> To reference columns from tables/subqueries in the `FROM` clause, you need to
> use fully qualified names, e.g. `users.id` instead of just `id`.
>
> The unqualified names are only allowed outside of the `SELECT` fields (but
> allowed in `WHERE`, `GROUP BY`, etc) and they resolve to the columns defined
> in `SELECT` fields.

The `$param` and `$.param` syntax is used for parameters.

Regular `$param` syntax is used to splice in expressions defined outside of the
query while `$.param` syntax is used to build expressions in the current query
scope:
```ocaml
# let users ~field ~where = {%q|
    SELECT users.id AS id, users.name AS name, $field::String AS extra_field
    FROM db.users
    WHERE users.is_active AND $.where
  |}
val users :
  field:(non_null, string) expr ->
  where:(< extra_field : (non_null, string) expr;
           id : (non_null, int number) expr; name : (non_null, string) expr;
           users : < id : (non_null, int number) expr;
                     is_active : (non_null, bool) expr;
                     name : (non_null, string) expr >
                   scope > ->
         (non_null, bool) expr) ->
  < extra_field : (non_null, string) expr; id : (non_null, int number) expr;
    name : (non_null, string) expr >
  scope select = <fun>
```

Finally to generate SQL from the query, one needs to define what exactly to
select and how to parse each column:
```ocaml
# let sql, parse_row = Ch_queries.query {%q|SELECT users.id FROM db.users|} Row.(fun __q -> col {%e|q.id|} Parse.int);;
val sql : string = "SELECT users.id AS id FROM public.users AS users"
val parse_row : json list -> int = <fun>
```

## `%e` - expressions, `%s` - scopes

There's a `%e` syntax form which allows you to define standalone expressions, which
can be spliced into queries later:
```ocaml
# let two = {%e|1+1|};;
val two : (non_null, int number) expr = <abstr>
```

Remember than query parameters are non just plain expressions but functions
which take the query scope and return an expression. It is useful (and
sometimes required) to annotate scope types explicitly. For that we have `%s`
syntax form:
```ocaml
# let is_deleted {%s|q (is_active Bool, ...)|} = {%e|NOT q.is_active|};;
val is_deleted :
  < q : < is_active : (non_null, bool) expr; .. > scope > ->
  (non_null, bool) expr = <fun>
```

Note that `q` in `q.is_active` is being resolved in the current scope, thus
reusable expressions are usually defined as functions from scopes to
expressions.

> [!IMPORTANT]
> In most cases it is required to annotate types of the arguments of reusable
> expressions with `_ Ch_queries.scope`. This is to force the type inference to
> infer the polymorphic type for scopes.

## `%eu` - expressions, unsafely

Sometimes you need to construct an expression using syntax which is not
supported by ch_queries.ppx. In this case you can use the `%eu` syntax:
```ocaml
# let expr {%s|q ...|} = {%eu|q.name || ' ' || q.surname|};;
val expr :
  < q : < name : ('a, 'b) expr; surname : ('c, 'd) expr; .. > scope > ->
  ('e, 'f) expr = <fun>
```

Such syntax recognizes only `q.name` and `$param` constructs and passes the rest
as-is.

> [!IMPORTANT]
> The parameters and the result of `%eu` expressions are inferred to have "any
> expression" type. Consider putting additional type constraints on them to
> avoid spreading unsafety to other parts of the code.

## `%t` - types

Finally, the `%t` syntax form is used as a shortcut to define DSL types:
```ocaml
# type ch_uint64 = {%t|UInt64|};;
type ch_uint64 = (non_null, uint64 number) expr
# type ch_nullable_string = {%t|Nullable(String)|};;
type ch_nullable_string = (null, string) expr
# type ch_array_string = {%t|Array(String)|};;
type ch_array_string = (non_null, (non_null, string) Ch_queries.array) expr
```

Can also be used for scope types:
```ocaml
# type user_scope = {%t| (id UInt64, name Nullable(String)) |};;
type user_scope =
    < id : (non_null, uint64 number) expr; name : (null, string) expr > scope
```

Scope types can also be acquired by referencing scopes of database tables:
```ocaml
# type users = {%t|db.users|};;
type users = Ch_database.Db.users scope
```
