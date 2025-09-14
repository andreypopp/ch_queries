# TODO

## improve %ch.select to support Custom(<ocamltype>)

We want to extend `%ch.select` to support `Custom(<ocamltype>)`.

First consider the following definition:
```ocaml
type domain = Domain of string
let domain_of_json = function
  | `String s -> Domain s
  | _ -> failwith "invalid domain, expected string"
```

Now the query:
```ocaml
let%ch.select domains = {|
  SELECT domain::Custom(domain) AS domain FROM domains
|}
```

Here the expanded code will be:
```ocaml
type domains_row = {domain: domain}
let domains = ...
```

Note that when staging the type annotation, it'll stage as `(_, _) Ch_queries.expr`.

For now we support only simple identifiers for `<ocamltype>`.

## improve %ch.select to support mapping to an existing record type

We want to extend `%ch.select` to support mapping to an existing record type.

For example, consider the following record type:
```ocaml
type user = {
  id: int;
  name: string;
  email: string option;
}
```
Now the query:
```ocaml
let%ch.select[@ch.row user] users = {|
  SELECT id, name, email FROM users
|}
```

## add type inference

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

## support table functions, they should accept OCaml expressions as arguments

An example:
```ocaml
let%ch.select users = {|
    SELECT * FROM users_with_permission(permission: `Read_write) AS users
|}

## Allow to attach WHERE/HAVING/GROUP BY/ORDER BY to FROM clauses

That way we can define "query templates" and then query them without introducing a subquery.
