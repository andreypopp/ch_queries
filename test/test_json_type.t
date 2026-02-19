JSON type in type position:
  $ ./compile_and_run '
  > type test_json = [%t "JSON"]
  > type test_nullable_json = [%t "Nullable(JSON)"]
  > '
  >>> PREPROCESSING
  type test_json = (Ch_queries.non_null, Ch_queries.json) Ch_queries.expr
  type test_nullable_json = (Ch_queries.null, Ch_queries.json) Ch_queries.expr
  >>> RUNNING

JSON expression ascription (preprocessing only):
  $ ./compile_and_run '
  > let x (y : [%t "JSON"]) = {%e|$y::JSON|}
  > '
  >>> PREPROCESSING
  let x (y : (Ch_queries.non_null, Ch_queries.json) Ch_queries.expr) =
    (y : (Ch_queries.non_null, Ch_queries.json) Ch_queries.expr)
  >>> RUNNING

JSON column in a query:
  $ ./compile_and_run '
  > let q u = {%q|SELECT u.data FROM $u::(data JSON)|};;
  > #show q
  > ' --run-only
  >>> RUNNING
  val q :
    (alias:string ->
     < data : (Ch_queries.non_null, Ch_queries.json) Ch_queries.expr >
     Ch_queries.scope Ch_queries.from_one) ->
    < data : (Ch_queries.non_null, Ch_queries.json) Ch_queries.expr >
    Ch_queries.scope Ch_queries.select

Nullable JSON column in a query:
  $ ./compile_and_run '
  > let q u = {%q|SELECT u.data FROM $u::(data Nullable(JSON))|};;
  > #show q
  > ' --run-only
  >>> RUNNING
  val q :
    (alias:string ->
     < data : (Ch_queries.null, Ch_queries.json) Ch_queries.expr >
     Ch_queries.scope Ch_queries.from_one) ->
    < data : (Ch_queries.null, Ch_queries.json) Ch_queries.expr >
    Ch_queries.scope Ch_queries.select

JSON type is parsed with Parse.json:
  $ ./compile_and_run '
  > let p : (Ch_queries.non_null, Ch_queries.json, Ch_queries.json) Ch_queries.Parse.t = Ch_queries.Parse.json
  > let v = Ch_queries.Parse.parse p (`String "hello")
  > let () = match v with `String "hello" -> () | _ -> failwith "unexpected"
  > ' --run-only
  >>> RUNNING

JSON in ch_queries parse (surface syntax):
  $ ch_queries parse 'SELECT u.data FROM $u WHERE u.data::JSON'
  SELECT u.data FROM u AS u WHERE u.data : JSON
