
joinGet with db.table dict and string value:
  $ ch_queries parse "SELECT joinGet(db.dict, 'value', t.key) FROM db.table AS t"
  SELECT joinGet(db.dict, 'value', t.key) FROM db.table AS t

joinGet with db.table dict and multiple keys:
  $ ch_queries parse "SELECT joinGet(db.dict, 'value', t.key1, t.key2) FROM db.table AS t"
  SELECT joinGet(db.dict, 'value', t.key1, t.key2) FROM db.table AS t

joinGet with $param dict:
  $ ch_queries parse "SELECT joinGet(\$dict, 'value', t.key) FROM db.table AS t"
  SELECT joinGet($dict, 'value', t.key) FROM db.table AS t

joinGet with $param value:
  $ ch_queries parse "SELECT joinGet(db.dict, \$value, t.key) FROM db.table AS t"
  SELECT joinGet(db.dict, $value, t.key) FROM db.table AS t

joinGet with both params:
  $ ch_queries parse "SELECT joinGet(\$dict, \$value, t.key1, t.key2) FROM db.table AS t"
  SELECT joinGet($dict, $value, t.key1, t.key2) FROM db.table AS t

joinGetOrNull:
  $ ch_queries parse "SELECT joinGetOrNull(db.dict, 'value', t.key) FROM db.table AS t"
  SELECT joinGetOrNull(db.dict, 'value', t.key) FROM db.table AS t

joinGet PPX transformation with 'db.table' string:
  $ ./compile_and_run '
  > let x = [%e "joinGet('\''public.dict'\'', '\''value'\'', 1)"];; #show x
  > ' --run-only
  >>> RUNNING
  val x : (Ch_queries.non_null, string) Ch_queries.expr

joinGet PPX transformation with $param dict:
  $ ./compile_and_run '
  > let x ~dict = [%e "joinGet($dict, '\''value'\'', 1)"];; #show x
  > ' --run-only
  >>> RUNNING
  val x :
    dict:((Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr,
          < value : ('a, 'b) Ch_queries.expr; .. >)
         Ch_queries.Dict.t ->
    ('a, 'b) Ch_queries.expr

joinGet PPX transformation with multiple keys:
  $ ./compile_and_run '
  > let x = {%e|joinGet(public.multikey_dict, '\''value'\'', 1, 2)|};; #show x
  > ' --run-only
  >>> RUNNING
  val x : (Ch_queries.non_null, string) Ch_queries.expr

joinGet PPX transformation with $param value:
  $ ./compile_and_run '
  > let x ~dict ~value = [%e "joinGet($dict, $value, 1)"];; #show x
  > let q = x ~dict:Ch_database.Public.dict ~value:(fun __q -> {%e|value|});; #show q
  > ' --run-only
  >>> RUNNING
  val x :
    dict:((Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr, 'a)
         Ch_queries.Dict.t ->
    value:('a -> ('b, 'c) Ch_queries.expr) -> ('b, 'c) Ch_queries.expr
  val q : (Ch_queries.non_null, string) Ch_queries.expr

joinGet PPX transformation with $param value:
  $ ./compile_and_run '
  > let x ~dict ~value = [%e "joinGet(public.dict, $value, 1)"];; #show x
  > let q = x ~value:(fun __q -> {%e|value|});; #show q
  > ' --run-only
  >>> RUNNING
  val x :
    dict:'a ->
    value:(< value : (Ch_queries.non_null, string) Ch_queries.expr > ->
           ('b, 'c) Ch_queries.expr) ->
    ('b, 'c) Ch_queries.expr
  val q : dict:'a -> (Ch_queries.non_null, string) Ch_queries.expr

joinGetOrNull PPX transformation:
  $ ./compile_and_run '
  > let x ~dict = [%e "joinGetOrNull($dict, '\''value'\'', 1)"]
  > '
  >>> PREPROCESSING
  let x ~dict =
    Ch_queries.Expr.joinGetOrNull
      (dict : _ Ch_queries.Dict.t)
      ((dict : _ Ch_queries.Dict.t).Ch_queries.Dict.values#query (fun __q ->
           __q#value))
      (Ch_queries.int 1)
  >>> RUNNING

joinGet error: no keys:
  $ ./compile_and_run '
  > let x ~dict = [%e "joinGet($dict, '\''value'\'')"]
  > '
  >>> PREPROCESSING
  File "-", line 2, characters 19-42:
  Error: joinGet requires at least 1 key argument
  [1]

joinGet error: too many keys:
  $ ./compile_and_run '
  > let x ~dict = [%e "joinGet($dict, '\''value'\'', 1, 2, 3, 4, 5)"]
  > '
  >>> PREPROCESSING
  File "-", line 2, characters 19-57:
  Error: joinGet supports at most 4 key arguments
  [1]
