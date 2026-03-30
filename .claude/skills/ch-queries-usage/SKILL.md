---
name: use ch_queries
description: Knows how to use ch_queries library and ppx to write typesafe ClickHouse SQL queries in OCaml
---

# ch_queries - Typesafe ClickHouse SQL in OCaml

## Overview

ch_queries is an OCaml library + PPX for generating typesafe ClickHouse SQL
queries. The PPX translates SQL-like surface syntax into typesafe OCaml
combinators. 

## Quick Reference

### PPX Extension Points

| Form | Purpose | Example |
|------|---------|---------|
| `%q` / `%query` | Define a query (returns `select`) | `{%q\|SELECT users.id FROM db.users\|}` |
| `%ch.query_and_map` | Query + row parser (returns `sql, map`) | `{%ch.query_and_map\|SELECT users.id::Int32 AS id FROM db.users\|}` |
| `%e` / `%expr` | Standalone expression | `{%e\|1 + 1\|}` |
| `%eu` | Unsafe expression (passthrough SQL) | `{%eu\|q.name \|\| ' ' \|\| q.surname\|}` |
| `%s` / `%scope` | Scope type annotation for function args | `{%s\|q (is_active Bool, ...)\|}` |
| `%t` / `%typ` | Type shortcut | `{%t\|Nullable(String)\|}` |

### Parameter Syntax

Inside query (`%q`) and expressions forms (`$e` and `%eu`) one can you use parameters to splice values into. The syntax is the following:

| Syntax | Meaning |
|--------|---------|
| `$param` | Splice an OCaml expression |
| `$.param` | Expression built from current query scope (receives scope as arg) |
| `$Mod.param` | Module-qualified splice |
| `$.Mod.param` | Module-qualified scope param |
| `?$param` | Optional param (clause omitted if `None`) |
| `?$.param` | Optional scope-aware param |
| `$param...` | Variadic splice (expands a list), only allowed for queries |

### Database Schema Definition

Queries reference tables via a schema module `Ch_database`. The schema
maps DB tables to OCaml definitions with scopes, describing columns.

```ocaml
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

### Query Examples

**Basic query:**
```ocaml
let users = {%q|SELECT users.id, users.name FROM db.users|}
```

**Query with parameters:**
```ocaml
let users ~field ~where = {%q|
  SELECT users.id AS id, users.name AS name, $field::String AS extra_field
  FROM db.users
  WHERE users.is_active AND $.where
|}
```

**Generating SQL / row parser from a query:**
```ocaml
let sql, parse_row =
  Ch_queries.query {%q|SELECT users.id FROM db.users|}
    Row.(fun __q -> col {%e|q.id|} Parse.int)
```

**Query and map:** a shortcut to generate SQL and a row parser that maps directly to a function

```ocaml
let sql, parse_row = {%ch.query_and_map|
  SELECT users.id::Int32 AS id, users.is_active::Bool AS is_active
  FROM db.users
|}
(* map : json list -> (id:int -> is_active:bool -> 'a) -> 'a *)
```

### Column References

- **Qualified names** (`users.id`) are required in `SELECT` fields
- **Unqualified names** (`id`) resolve to SELECT aliases and are allowed in `WHERE`, `GROUP BY`, etc.
- It is recommended to provide column aliases in `SELECT`, otherwise automatically generated names are meaningless

### Type Annotations

Use `::Type` to ascribe types (required in `%query_and_map`):
- `::Int32`, `::UInt64`, `::String`, `::Bool`, `::Float64`
- `::Nullable(String)`, `::Array(String)`
- `::Date`, `::DateTime`, `::JSON`
- Fields without annotation default to `Any` (raw JSON)

### Joins

```ocaml
(* INNER JOIN *)
{%q|SELECT u.id, p.name FROM db.users AS u JOIN db.profiles AS p ON u.id = p.user_id|}

(* LEFT JOIN - right side becomes nullable *)
{%q|SELECT u.id, p.name FROM db.users AS u LEFT JOIN db.profiles AS p ON u.id = p.user_id|}
```

### CTEs (WITH)

```ocaml
(* Basic CTE *)
{%q|WITH active AS (SELECT users.id AS id FROM db.users WHERE users.is_active)
    SELECT active.id FROM $active|}

(* MATERIALIZED CTE - add MATERIALIZED keyword *)
```

### UNION

```ocaml
(* UNION ALL *)
let combined = Ch_queries.union
  {%q|SELECT 1 AS x FROM db.users|}
  {%q|SELECT 2 AS x FROM db.users|}
```

### Clauses

```ocaml
{%q|SELECT users.x AS x FROM db.users
    PREWHERE users.is_active          (* ClickHouse specific *)
    WHERE users.id > 10
    GROUP BY users.x
    HAVING count() > 5
    QUALIFY row_number() = 1
    ORDER BY users.x ASC
    LIMIT 100
    OFFSET 0
    SETTINGS max_threads=4
|}
```

### Optional Parameters

Optional params allowed immediately in `WHERE` and other clauses, clauses are omitted when params eval to `None`:

```ocaml
let users ?where ?limit () = {%q|
  SELECT users.x AS x FROM db.users
  WHERE ?$.where
  LIMIT ?$.limit
|}
(* Calling without args: WHERE and LIMIT are omitted *)
(* Calling with ~where:(fun __q -> ...) adds the WHERE clause *)
```

Optional params work in: `WHERE`, `HAVING`, `QUALIFY`, `LIMIT`, `OFFSET`, `ORDER BY`.

### ORDER BY WITH FILL

```ocaml
{%q|SELECT users.x AS x FROM db.users
    ORDER BY users.x WITH FILL FROM 0 TO 100 STEP 5|}

(* With INTERPOLATE *)
{%q|SELECT users.x AS x, users.id AS id FROM db.users
    ORDER BY users.x WITH FILL FROM 0 TO 10 INTERPOLATE (id AS 0)|}
```

### SETTINGS

```ocaml
(* Literal settings *)
{%q|SELECT users.x FROM db.users SETTINGS max_threads=4, enable_analyzer=false|}

(* Parameterized settings *)
{%q|SELECT users.x FROM db.users SETTINGS max_threads=$max_threads|}

(* Variadic splice *)
{%q|SELECT users.x FROM db.users SETTINGS $settings...|}
```

### Cluster Syntax

```ocaml
{%q|SELECT q.x FROM cluster(my_cluster, view(SELECT t.x FROM db.table AS t)) AS q|}

(* With parameterized cluster name *)
{%q|SELECT q.x FROM cluster($cluster_name, view(SELECT t.x FROM db.table AS t)) AS q|}
```

### Reusable Expressions with Scope Annotations

```ocaml
(* Define a reusable filter *)
let is_deleted {%s|q (is_active Bool, ...), ...|} = {%e|NOT q.is_active|}

(* Use in a query *)
let users = {%q|SELECT users.id AS id FROM db.users WHERE $.is_deleted|}
```

it is advised to keep scope annotations open (`...`) to maintain polymorphism and reusability of expressions across different queries.

### Conditionals

```ocaml
(* if(cond, then, else) *)
{%e|if(true, 'then', 'else')|}

(* multiIf(cond1, val1, cond2, val2, ..., else) *)
{%e|multiIf(true, 'a', false, 'b', 'default')|}
```

### Array / Lambda Expressions

```ocaml
{%e|arrayMap(x -> x + 1, [1, 2, 3])|}
{%e|arrayFilter(item -> item > 0, [1, -2, 3])|}
```

### ClickHouse Parameters (CH_PARAM)

For ClickHouse native query parameters:
```sql
SELECT {user:int} FROM users
```

### GROUP BY with GROUPING SETS

```ocaml
{%q|SELECT u.x AS x, u.id AS id FROM db.users AS u
    GROUP BY GROUPING SETS ((u.x, u.id), (u.id))|}
```

## Key Rules

1. **Always use qualified column refs in SELECT**: `users.id`, not just `id`
2. **`$.param` receives the query scope** - the param must be a function `scope -> expr`
3. **`$param` splices directly** - must already be an expression of the right type
4. **LEFT JOIN makes the right side nullable** - column types change from `non_null` to `null`
5. **Type annotations (`::Type`) are required** in `%query_and_map`'s `SELECT` for parsing
6. **Scope annotations** (`%s`) should use `...` for open types to keep them polymorphic
