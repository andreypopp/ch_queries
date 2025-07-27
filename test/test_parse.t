
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
