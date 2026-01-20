Testing ClickHouse functions over timestamps:

  $ ./compile_and_run "
  > let t = {%e|toDate('2024-01-01')|};; #show t
  > let t = {%e|toDateTime('2024-01-01 12:34:56')|};; #show t
  > let t = {%e|toDate('2024-01-01') > toDate('2022-01-01')|};; #show t
  > let t = {%e|toDateTime('2024-01-01') > toDateTime('2022-01-01')|};; #show t
  > let t = {%e|now()|};; #show t
  > let t = {%e|today()|};; #show t
  > let t = {%e|yesterday()|};; #show t
  > let t = {%e|addDays(toDate('2024-01-01'), 5)|};; #show t
  > let t = {%e|addHours(toDateTime('2024-01-01 12:00:00'), 3)|};; #show t
  > let t = {%e|addMinutes(toDateTime('2024-01-01 12:00:00'), 30)|};; #show t
  > let t = {%e|addMonths(toDate('2024-01-01'), 2)|};; #show t
  > let t = {%e|addSeconds(toDateTime('2024-01-01 12:00:00'), 45)|};; #show t
  > let t = {%e|addWeeks(toDate('2024-01-01'), 1)|};; #show t
  > let t = {%e|addYears(toDate('2024-01-01'), 1)|};; #show t
  > let t = {%e|subtractDays(toDate('2024-01-01'), 5)|};; #show t
  > let t = {%e|subtractHours(toDateTime('2024-01-01 12:00:00'), 3)|};; #show t
  > let t = {%e|subtractMinutes(toDateTime('2024-01-01 12:00:00'), 30)|};; #show t
  > let t = {%e|subtractMonths(toDate('2024-01-01'), 2)|};; #show t
  > let t = {%e|subtractSeconds(toDateTime('2024-01-01 12:00:00'), 45)|};; #show t
  > let t = {%e|subtractWeeks(toDate('2024-01-01'), 1)|};; #show t
  > let t = {%e|subtractYears(toDate('2024-01-01'), 1)|};; #show t
  > let t = {%e|toYYYYMM(toDate('2024-01-01'))|};; #show t
  > let t = {%e|toYYYYMMDD(toDate('2024-01-01'))|};; #show t
  > let t = {%e|toStartOfYear(toDate('2024-06-15'))|};; #show t
  > let t = {%e|toStartOfMonth(toDate('2024-06-15'))|};; #show t
  > let t = {%e|toStartOfWeek(toDate('2024-06-15'))|};; #show t
  > let t = {%e|toStartOfDay(toDateTime('2024-06-15 14:30:00'))|};; #show t
  > let t = {%e|toStartOfHour(toDateTime('2024-06-15 14:30:00'))|};; #show t
  > let t = {%e|toStartOfMinute(toDateTime('2024-06-15 14:30:45'))|};; #show t
  > let t = {%e|fromUnixTimestamp(1704067200)|};; #show t
  > let t = {%e|toIntervalMinute(30)|};; #show t
  > let t = {%e|toIntervalHour(2)|};; #show t
  > let t = {%e|toIntervalDay(7)|};; #show t
  > let t = {%e|toIntervalWeek(1)|};; #show t
  > let t = {%e|toIntervalMonth(3)|};; #show t
  > let t = {%e|toIntervalYear(1)|};; #show t
  > let t = {%e|addDate(toDate('2024-01-01'), INTERVAL 3 YEAR)|};; #show t
  > let t = {%e|addDate(toDateTime('2024-01-01 12:00:00'), INTERVAL 1 MONTH)|};; #show t
  > let t = {%e|subDate(toDate('2024-01-01'), INTERVAL 1 WEEK)|};; #show t
  > let t = {%e|addInterval(toDate('2024-01-01'), INTERVAL 5 DAY)|};; #show t
  > let t = {%e|toStartOfInterval(toDateTime('2024-06-15 14:35:22'), INTERVAL 15 MINUTE)|};; #show t
  > let t = {%e|now64()|};; #show t
  > let t = {%e|subtractInterval(toDate('2024-01-01'), INTERVAL 2 MONTH)|};; #show t
  > " --run-only
  >>> RUNNING
  val t : (Ch_queries.non_null, Ch_queries.date) Ch_queries.expr
  val t : (Ch_queries.non_null, Ch_queries.datetime) Ch_queries.expr
  val t : (Ch_queries.non_null, bool) Ch_queries.expr
  val t : (Ch_queries.non_null, bool) Ch_queries.expr
  val t : (Ch_queries.non_null, Ch_queries.datetime) Ch_queries.expr
  val t : (Ch_queries.non_null, Ch_queries.date) Ch_queries.expr
  val t : (Ch_queries.non_null, Ch_queries.date) Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.date0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.datetime0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.datetime0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.date0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.datetime0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.date0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.date0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.date0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.datetime0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.datetime0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.date0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.datetime0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.date0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.date0 Ch_queries.timestamp)
    Ch_queries.expr
  val t : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  val t : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.date0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.date0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.date0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.datetime0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.datetime0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.datetime0 Ch_queries.timestamp)
    Ch_queries.expr
  val t : (Ch_queries.non_null, Ch_queries.datetime) Ch_queries.expr
  val t : (Ch_queries.non_null, Ch_queries.interval) Ch_queries.expr
  val t : (Ch_queries.non_null, Ch_queries.interval) Ch_queries.expr
  val t : (Ch_queries.non_null, Ch_queries.interval) Ch_queries.expr
  val t : (Ch_queries.non_null, Ch_queries.interval) Ch_queries.expr
  val t : (Ch_queries.non_null, Ch_queries.interval) Ch_queries.expr
  val t : (Ch_queries.non_null, Ch_queries.interval) Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.date0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.datetime0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.date0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.date0 Ch_queries.timestamp)
    Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.datetime0 Ch_queries.timestamp)
    Ch_queries.expr
  val t : (Ch_queries.non_null, Ch_queries.datetime64) Ch_queries.expr
  val t :
    (Ch_queries.non_null, Ch_queries.date0 Ch_queries.timestamp)
    Ch_queries.expr
