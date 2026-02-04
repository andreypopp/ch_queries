
Tokenizing float literals:
  $ ch_queries tokenize "3.14"
  FLOAT(3.14) line 1, col 1-5
  EOF line 1, col 5-5

  $ ch_queries tokenize "0.5"
  FLOAT(0.5) line 1, col 1-4
  EOF line 1, col 4-4

  $ ch_queries tokenize ".5"
  FLOAT(0.5) line 1, col 1-3
  EOF line 1, col 3-3

  $ ch_queries tokenize "3."
  FLOAT(3) line 1, col 1-3
  EOF line 1, col 3-3

Parsing float literals in expressions:
  $ ch_queries parse 'SELECT 3.14 FROM $x'
  SELECT 3.14 FROM x AS x

  $ ch_queries parse 'SELECT t.price * 1.5 FROM db.table AS t'
  SELECT t.price * 1.5 FROM db.table AS t

Negative floats via unary minus:
  $ ch_queries parse 'SELECT -3.14 FROM $x'
  SELECT negate(3.14) FROM x AS x

Floats in arithmetic:
  $ ch_queries parse 'SELECT 3.14 + 2.0 FROM $x'
  SELECT 3.14 + 2. FROM x AS x

  $ ch_queries parse 'SELECT 10 - 3.5 FROM $x'
  SELECT 10 - 3.5 FROM x AS x

Floats in settings:
  $ ch_queries parse 'SELECT t.x FROM db.table AS t SETTINGS timeout = 3.5'
  SELECT t.x FROM db.table AS t SETTINGS timeout=3.5
