Test unsafe expressions with basic functionality:

Test parsing expressions:

  $ ch_queries parse-expr 'unsafe { raw_sql }'
  raw_sql 

  $ ch_queries parse-expr 'unsafe { $param }'
  $param

  $ ch_queries parse-expr 'unsafe { table.column }'
  table.column

  $ ch_queries parse-expr 'unsafe { $param, table.column, raw_sql }'
  $param, table.column, raw_sql 

  $ ch_queries parse-expr 'unsafe { nested { braces } work }'
  nested { braces } work 

Test parsing queries:

  $ ch_queries parse 'SELECT unsafe { custom_expr } FROM table'
  SELECT custom_expr  FROM table AS table

  $ ch_queries parse 'SELECT 1 FROM table WHERE unsafe { condition }'
  SELECT 1 FROM table AS table WHERE condition 
