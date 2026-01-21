this showcases different parse/unparse combinators:
  $ ./compile_and_run '
  > let int64 = Ch_queries.Parse.(unparse int64) 42L;;
  > let uint64 = Ch_queries.Parse.(unparse uint64) (Unsigned.UInt64.of_int64 42L);;
  > let date = Ch_queries.Parse.(unparse date) 0.;;
  > let datetime = Ch_queries.Parse.(unparse datetime) 0.;;
  > let int_opt = Ch_queries.Parse.(unparse (nullable int)) (Some 1);;
  > let int_opt_null = Ch_queries.Parse.(unparse (nullable int)) None;;
  > let x __q = [%eu "tuple($int64, $uint64, $date, $datetime, $int_opt, $int_opt_null)"];;
  > let q = [%q "SELECT $.x AS x FROM public.users"];;
  > let sql, _parse_row = Ch_queries.query q @@ fun __q -> Ch_queries.Row.ignore [%e "q.x"]
  > let () = print_endline sql;;
  > ' --run-only
  >>> RUNNING
  SELECT
    tuple(toInt64(42), toUInt64(42), toDate('1970-01-01'), toDateTime('1970-01-01T00:00:00'), toNullable(1), NULL)
      AS _1
  FROM public.users AS users
