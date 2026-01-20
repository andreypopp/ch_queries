
dictGet parsing with db.table dict:
  $ ch_queries parse "SELECT dictGet(db.dict, 'value', t.key) FROM db.table AS t"
  SELECT dictGet(db.dict, 'value', t.key) FROM db.table AS t

dictGet parsing with $param dict:
  $ ch_queries parse "SELECT dictGet(\$dict, 'value', t.key) FROM db.table AS t"
  SELECT dictGet($dict, 'value', t.key) FROM db.table AS t

dictGet PPX transformation with 'db.table' string:
  $ ./compile_and_run '
  > let x = [%e "dictGet('\''public.dict'\'', '\''value'\'', 1)"];; #show x
  > ' --run-only
  >>> RUNNING
  val x : (Ch_queries.non_null, string) Ch_queries.expr

dictGet PPX transformation:
  $ ./compile_and_run '
  > let x = [%e "dictGet(public.dict, '\''value'\'', 1)"];; #show x
  > ' --run-only
  >>> RUNNING
  val x : (Ch_queries.non_null, string) Ch_queries.expr

  $ ./compile_and_run '
  > let x = [%e "dictGet(public.multikey_dict, '\''value'\'', tuple(1, 2))"];; #show x
  > ' --run-only
  >>> RUNNING
  val x : (Ch_queries.non_null, string) Ch_queries.expr
