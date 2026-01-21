
Comments are ignored by the lexer:

  $ ch_queries tokenize "SELECT x -- comment"
  SELECT line 1, col 1-7
  ID(x) line 1, col 8-9
  EOF line 1, col 20-20

  $ ch_queries tokenize "SELECT x -- comment
  > FROM t"
  SELECT line 1, col 1-7
  ID(x) line 1, col 8-9
  FROM line 2, col 1-5
  ID(t) line 2, col 6-7
  EOF line 2, col 7-7

  $ ch_queries parse 'SELECT x FROM db.t -- comment'
  SELECT x FROM db.t AS t

  $ ch_queries parse 'SELECT
  > x -- selecting x column
  > FROM db.t'
  SELECT x FROM db.t AS t
