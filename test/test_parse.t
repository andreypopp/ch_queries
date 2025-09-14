
  $ ch_queries parse 'SELECT x.name FROM $x'
  SELECT x.name FROM x AS x

  $ ch_queries parse 'SELECT x.name FROM $x WHERE x.age + 30'
  SELECT x.name FROM x AS x WHERE x.age + 30

  $ ch_queries parse 'SELECT x.name FROM $x WHERE x.is_active'
  SELECT x.name FROM x AS x WHERE x.is_active

  $ ch_queries parse 'SELECT x.name FROM $x WHERE true'
  SELECT x.name FROM x AS x WHERE true

parsing AND:
  $ ch_queries parse 'SELECT x.name FROM $x WHERE true AND false'
  SELECT x.name FROM x AS x WHERE true AND false

parsing OR:
  $ ch_queries parse 'SELECT x.name FROM $x WHERE true OR false'
  SELECT x.name FROM x AS x WHERE true OR false

parsing splicing:
  $ ch_queries parse 'SELECT x.name FROM $x WHERE $condition'
  SELECT x.name FROM x AS x WHERE $condition

parsing string literals:
  $ ch_queries parse "SELECT x.name FROM \$x WHERE x.name = 'hello'"
  SELECT x.name FROM x AS x WHERE x.name = 'hello'

  $ ch_queries parse "SELECT 'hello world' FROM \$x"
  SELECT 'hello world' FROM x AS x

  $ ch_queries parse "SELECT 'string with \\'quotes\\'' FROM \$x"
  SELECT 'string with \'quotes\'' FROM x AS x

  $ ch_queries parse "SELECT 'string with\nnewline' FROM \$x"
  SELECT 'string with\nnewline' FROM x AS x

  $ ch_queries parse "SELECT 'string with\ttab' FROM \$x"
  SELECT 'string with\ttab' FROM x AS x

parsing GROUP BY:
  $ ch_queries parse 'SELECT x.name FROM $x GROUP BY x.name'
  SELECT x.name FROM x AS x GROUP BY x.name

  $ ch_queries parse 'SELECT x.name FROM $x GROUP BY x.name, x.age'
  SELECT x.name FROM x AS x GROUP BY x.name, x.age

parsing ORDER BY:
  $ ch_queries parse 'SELECT x.name FROM $x ORDER BY x.name'
  SELECT x.name FROM x AS x ORDER BY x.name ASC

  $ ch_queries parse 'SELECT x.name FROM $x ORDER BY x.name DESC'
  SELECT x.name FROM x AS x ORDER BY x.name DESC

  $ ch_queries parse 'SELECT x.name FROM $x ORDER BY x.name ASC'
  SELECT x.name FROM x AS x ORDER BY x.name ASC

  $ ch_queries parse 'SELECT x.name FROM $x ORDER BY x.name, x.age'
  SELECT x.name FROM x AS x ORDER BY x.name ASC, x.age ASC

parsing cluster syntax with literal cluster name:
  $ ch_queries parse 'SELECT q.x FROM cluster(my_cluster, view(SELECT t.x FROM db.table as t)) AS q'
  SELECT q.x FROM cluster(my_cluster, view(SELECT t.x FROM db.table AS t)) AS q

parsing cluster syntax with parameterized cluster name:
  $ ch_queries parse 'SELECT q.x FROM cluster($cluster_name, view(SELECT t.x FROM db.table as t)) AS q'
  SELECT q.x FROM cluster(?cluster_name, view(SELECT t.x FROM db.table AS t)) AS q

parsing arrays:
  $ ch_queries parse 'SELECT [] as x FROM db.table'
  SELECT [] AS x FROM db.table AS table

  $ ch_queries parse 'SELECT [1, 2] as x FROM db.table'
  SELECT [1, 2] AS x FROM db.table AS table

parsing lambda expressions:
  $ ch_queries parse 'SELECT arrayMap(x -> x + 1, [1, 2, 3]) FROM db.table'
  SELECT arrayMap((x -> (x + 1)), [1, 2, 3]) FROM db.table AS table

  $ ch_queries parse 'SELECT arrayFilter(item -> item + 1, t.numbers) FROM db.table AS t'
  SELECT arrayFilter((item -> (item + 1)), t.numbers) FROM db.table AS t

  $ ch_queries parse 'SELECT arrayReduce(x -> x * 2, t.arr) FROM db.table AS t'
  SELECT arrayReduce((x -> (x * 2)), t.arr) FROM db.table AS t

parsing nested lambda expressions:
  $ ch_queries parse 'SELECT arrayMap(x -> arrayMap(y -> x + y, [1, 2]), [3, 4]) FROM db.table'
  SELECT arrayMap((x -> (arrayMap((y -> (x + y)), [1, 2]))), [3, 4])
  FROM db.table AS table

parsing lambda with complex body:
  $ ch_queries parse 'SELECT arrayMap(x -> x * 2 + 1, t.nums) FROM db.table AS t'
  SELECT arrayMap((x -> (x * 2 + 1)), t.nums) FROM db.table AS t

parsing lambda with parentheses:
  $ ch_queries parse 'SELECT arrayMap((x -> x + 1), t.arr) FROM db.table AS t'
  SELECT arrayMap((x -> (x + 1)), t.arr) FROM db.table AS t
