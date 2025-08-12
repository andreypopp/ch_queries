testing in queries:
  $ ch_queries parse 'SELECT x FROM ?table WHERE x != 1'
  SELECT x FROM table AS table WHERE x != 1

  $ ch_queries parse 'SELECT x FROM ?table WHERE x <> 1'
  SELECT x FROM table AS table WHERE x != 1

testing with other operators:
  $ ch_queries parse-expr 'x != 1 AND y = 2'
  x != 1 AND y = 2

  $ ch_queries parse-expr '(x != 1) OR (y <> 2)'
  x != 1 OR y != 2
