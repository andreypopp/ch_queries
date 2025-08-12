basic interval expressions (plural forms):
  $ ./compile_and_run '
  > let x = [%e "INTERVAL 5 DAYS"]
  > '
  >>> PREPROCESSING
  let x = Ch_queries.interval 5 `DAY
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "INTERVAL 30 MINUTES"]
  > '
  >>> PREPROCESSING
  let x = Ch_queries.interval 30 `MINUTE
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "INTERVAL 2 WEEKS"]
  > '
  >>> PREPROCESSING
  let x = Ch_queries.interval 2 `WEEK
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "INTERVAL 2 YEARS"]
  > '
  >>> PREPROCESSING
  let x = Ch_queries.interval 2 `YEAR
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "INTERVAL 12 MONTHS"]
  > '
  >>> PREPROCESSING
  let x = Ch_queries.interval 12 `MONTH
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "INTERVAL 3 HOURS"]
  > '
  >>> PREPROCESSING
  let x = Ch_queries.interval 3 `HOUR
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "INTERVAL 45 SECONDS"]
  > '
  >>> PREPROCESSING
  let x = Ch_queries.interval 45 `SECOND
  >>> RUNNING

basic interval expressions (singular forms):
  $ ./compile_and_run '
  > let x = [%e "INTERVAL 1 DAY"]
  > '
  >>> PREPROCESSING
  let x = Ch_queries.interval 1 `DAY
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "INTERVAL 1 MINUTE"]
  > '
  >>> PREPROCESSING
  let x = Ch_queries.interval 1 `MINUTE
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "INTERVAL 1 WEEK"]
  > '
  >>> PREPROCESSING
  let x = Ch_queries.interval 1 `WEEK
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "INTERVAL 1 YEAR"]
  > '
  >>> PREPROCESSING
  let x = Ch_queries.interval 1 `YEAR
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "INTERVAL 1 MONTH"]
  > '
  >>> PREPROCESSING
  let x = Ch_queries.interval 1 `MONTH
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "INTERVAL 1 HOUR"]
  > '
  >>> PREPROCESSING
  let x = Ch_queries.interval 1 `HOUR
  >>> RUNNING

  $ ./compile_and_run '
  > let x = [%e "INTERVAL 1 SECOND"]
  > '
  >>> PREPROCESSING
  let x = Ch_queries.interval 1 `SECOND
  >>> RUNNING

interval with function calls:
  $ ./compile_and_run '
  > let x = [%e "addInterval(now(), INTERVAL 2 WEEKS)"]
  > '
  >>> PREPROCESSING
  let x =
    Ch_queries.Expr.addInterval (Ch_queries.Expr.now ())
      (Ch_queries.interval 2 `WEEK)
  >>> RUNNING
