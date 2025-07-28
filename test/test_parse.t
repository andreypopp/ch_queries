
  $ queries parse 'SELECT x.name FROM x'
  SELECT x.name FROM x AS x

  $ queries parse 'SELECT x.name FROM x WHERE x.age + 30'
  SELECT x.name FROM x AS x WHERE x.age + 30

  $ queries parse 'SELECT x.name FROM x WHERE x.is_active'
  SELECT x.name FROM x AS x WHERE x.is_active

  $ queries parse 'SELECT x.name FROM x WHERE true'
  SELECT x.name FROM x AS x WHERE true

parsing AND:
  $ queries parse 'SELECT x.name FROM x WHERE true AND false'
  SELECT x.name FROM x AS x WHERE true AND false

parsing OR:
  $ queries parse 'SELECT x.name FROM x WHERE true OR false'
  SELECT x.name FROM x AS x WHERE true OR false

parsing splicing:
  $ queries parse 'SELECT x.name FROM x WHERE condition'
  SELECT x.name FROM x AS x WHERE condition

parsing string literals:
  $ queries parse "SELECT x.name FROM x WHERE x.name = 'hello'"
  SELECT x.name FROM x AS x WHERE x.name = 'hello'

  $ queries parse "SELECT 'hello world' FROM x"
  SELECT 'hello world' FROM x AS x

  $ queries parse "SELECT 'string with \\'quotes\\'' FROM x"
  SELECT 'string with \'quotes\'' FROM x AS x

  $ queries parse "SELECT 'string with\nnewline' FROM x"
  SELECT 'string with\nnewline' FROM x AS x

  $ queries parse "SELECT 'string with\ttab' FROM x"
  SELECT 'string with\ttab' FROM x AS x

parsing GROUP BY:
  $ queries parse 'SELECT x.name FROM x GROUP BY x.name'
  SELECT x.name FROM x AS x GROUP BY x.name

  $ queries parse 'SELECT x.name FROM x GROUP BY x.name, x.age'
  SELECT x.name FROM x AS x GROUP BY x.name, x.age

paersing ORDER BY:
  $ queries parse 'SELECT x.name FROM x ORDER BY x.name'
  SELECT x.name FROM x AS x ORDER BY x.name ASC
 
  $ queries parse 'SELECT x.name FROM x ORDER BY x.name DESC'
  SELECT x.name FROM x AS x ORDER BY x.name DESC

  $ queries parse 'SELECT x.name FROM x ORDER BY x.name ASC'
  SELECT x.name FROM x AS x ORDER BY x.name ASC

  $ queries parse 'SELECT x.name FROM x ORDER BY x.name, x.age'
  SELECT x.name FROM x AS x ORDER BY x.name ASC, x.age ASC
