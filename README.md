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

module Database = struct
  module Db = struct
    let users =
      let scope ~alias =
        let column name = unsafe (alias ^ "." ^ name) in
        object
          method id : (non_null, int number) expr = column "id"
          method name : (non_null, string) expr = column "name"
          method is_active : (non_null, bool) expr = column "is_active"
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

The `?param` syntax is used for parameters. If a parameter appears in the
expression position, then it is expected to be a function which takes the current
scope and returns an expression:
```ocaml
# let users ~where = {%q|
    SELECT users.id AS id, users.name AS name
    FROM db.users
    WHERE users.is_active AND ?where
  |}
val users :
  where:(< id : (non_null, int number) expr;
           is_active : (non_null, bool) expr;
           name : (non_null, string) expr >
         scope -> (non_null, bool) expr) ->
  < id : (non_null, int number) expr; name : (non_null, string) expr > scope
  select = <fun>
```

Finally to generate SQL from the query, one needs to define what exactly to
select and how to parse each column:
```ocaml
# let sql, parse_row = Ch_queries.query {%q|SELECT id FROM db.users|} Row.(fun q -> int {%e|q.id|});;
val sql : string =
  "SELECT q._1 FROM (SELECT users.id AS _1 FROM public.users AS users) AS q"
val parse_row : json list -> int = <fun>
```

## `%e` - expressions

There's a `%e` syntax form which allows you to define standalone expressions, which
can be spliced into queries later:
```ocaml
# let ok q = {%e|q.is_active|};;
val ok : < query : (< is_active : 'a; .. > -> 'a) -> 'b; .. > -> 'b = <fun>
```

## `%eu` - expressions, unsafely

Sometimes you need to construct an expression using syntax which is not
supported by ch_queries.ppx. In this case you can use the `%eu` syntax:
```ocaml
# let expr q = {%eu|q.name || ' ' || q.surname|};;
val expr :
  < query : (< name : 'a; surname : 'a; .. > -> 'a) -> ('b, 'c) expr; .. > ->
  ('d, 'e) expr = <fun>
```

Such syntax recognizes only `q.name` and `?param` constructs and passes the rest
as-is.

## `%t` - types

Finally, the `%t` syntax form is used as a shortcut to define DSL types:
```ocaml
# type ch_uint64 = {%t|UInt64|};;
type ch_uint64 = (non_null, int64 number) expr
# type ch_nullable_string = {%t|Nullable(String)|};;
type ch_nullable_string = (null, string) expr
# type ch_array_string = {%t|Array(String)|};;
type ch_array_string = (non_null, (non_null, string) Ch_queries.array) expr
```
