Arithmetic operator precedence tests - multiplication binds tighter than addition:
  $ ch_queries parse 'SELECT a + b * c FROM ?x'
  SELECT a + b * c FROM x AS x

  $ ch_queries parse 'SELECT a * b + c FROM ?x'
  SELECT a * b + c FROM x AS x

  $ ch_queries parse 'SELECT a - b * c FROM ?x'
  SELECT a - b * c FROM x AS x

  $ ch_queries parse 'SELECT a * b - c FROM ?x'
  SELECT a * b - c FROM x AS x

Division binds tighter than addition/subtraction:
  $ ch_queries parse 'SELECT a + b / c FROM ?x'
  SELECT a + b / c FROM x AS x

  $ ch_queries parse 'SELECT a / b + c FROM ?x'
  SELECT a / b + c FROM x AS x

  $ ch_queries parse 'SELECT a - b / c FROM ?x'
  SELECT a - b / c FROM x AS x

  $ ch_queries parse 'SELECT a / b - c FROM ?x'
  SELECT a / b - c FROM x AS x

Multiplication and division have same precedence - left associative:
  $ ch_queries parse 'SELECT a * b / c FROM ?x'
  SELECT a * b / c FROM x AS x

  $ ch_queries parse 'SELECT a / b * c FROM ?x'
  SELECT a / b * c FROM x AS x

Addition and subtraction have same precedence - left associative:
  $ ch_queries parse 'SELECT a + b - c FROM ?x'
  SELECT a + b - c FROM x AS x

  $ ch_queries parse 'SELECT a - b + c FROM ?x'
  SELECT a - b + c FROM x AS x

Right associativity for subtraction and division (to avoid confusion):
  $ ch_queries parse 'SELECT a - (b - c) FROM ?x'
  SELECT a - (b - c) FROM x AS x

  $ ch_queries parse 'SELECT a / (b / c) FROM ?x'
  SELECT a / (b / c) FROM x AS x

Logical operator precedence - AND binds tighter than OR:
  $ ch_queries parse 'SELECT x FROM ?x WHERE a OR b AND c'
  SELECT x FROM x AS x WHERE a OR b AND c

  $ ch_queries parse 'SELECT x FROM ?x WHERE a AND b OR c'
  SELECT x FROM x AS x WHERE a AND b OR c

Comparison operators have higher precedence than logical operators:
  $ ch_queries parse 'SELECT x FROM ?x WHERE a = b AND c = d'
  SELECT x FROM x AS x WHERE a = b AND c = d

  $ ch_queries parse 'SELECT x FROM ?x WHERE a = b OR c = d'
  SELECT x FROM x AS x WHERE a = b OR c = d

  $ ch_queries parse 'SELECT x FROM ?x WHERE a > b AND c < d'
  SELECT x FROM x AS x WHERE a > b AND c < d

Arithmetic has higher precedence than comparison:
  $ ch_queries parse 'SELECT x FROM ?x WHERE a + b = c * d'
  SELECT x FROM x AS x WHERE a + b = c * d

  $ ch_queries parse 'SELECT x FROM ?x WHERE a * b > c + d'
  SELECT x FROM x AS x WHERE a * b > c + d

  $ ch_queries parse 'SELECT x FROM ?x WHERE a - b < c / d'
  SELECT x FROM x AS x WHERE a - b < c / d

Complex mixed expressions:
  $ ch_queries parse 'SELECT x FROM ?x WHERE a + b * c = d AND e OR f'
  SELECT x FROM x AS x WHERE a + b * c = d AND e OR f

  $ ch_queries parse 'SELECT x FROM ?x WHERE a OR b AND c + d * e = f'
  SELECT x FROM x AS x WHERE a OR b AND c + d * e = f

  $ ch_queries parse 'SELECT x FROM ?x WHERE a > b + c AND d * e < f OR g'
  SELECT x FROM x AS x WHERE a > b + c AND d * e < f OR g

Parentheses override precedence:
  $ ch_queries parse 'SELECT x FROM ?x WHERE (a + b) * c'
  SELECT x FROM x AS x WHERE (a + b) * c

  $ ch_queries parse 'SELECT x FROM ?x WHERE a * (b + c)'
  SELECT x FROM x AS x WHERE a * (b + c)

  $ ch_queries parse 'SELECT x FROM ?x WHERE (a OR b) AND c'
  SELECT x FROM x AS x WHERE (a OR b) AND c

  $ ch_queries parse 'SELECT x FROM ?x WHERE a AND (b OR c)'
  SELECT x FROM x AS x WHERE a AND (b OR c)

Lambda expressions (lowest precedence):
  $ ch_queries parse 'SELECT x FROM ?x WHERE p -> a + b'
  SELECT x FROM x AS x WHERE p -> a + b

  $ ch_queries parse 'SELECT x FROM ?x WHERE p -> a AND b'
  SELECT x FROM x AS x WHERE p -> a AND b

  $ ch_queries parse 'SELECT x FROM ?x WHERE (p -> a) + b'
  SELECT x FROM x AS x WHERE (p -> a) + b

Edge cases - deeply nested expressions:
  $ ch_queries parse 'SELECT a + b * c + d FROM ?x'
  SELECT a + b * c + d FROM x AS x

  $ ch_queries parse 'SELECT a * b + c * d FROM ?x'
  SELECT a * b + c * d FROM x AS x

  $ ch_queries parse 'SELECT x FROM ?x WHERE a AND b OR c AND d'
  SELECT x FROM x AS x WHERE a AND b OR c AND d

  $ ch_queries parse 'SELECT x FROM ?x WHERE a OR b AND c OR d'
  SELECT x FROM x AS x WHERE a OR b AND c OR d

Simple expressions should not have unnecessary parentheses:
  $ ch_queries parse 'SELECT x FROM ?x WHERE a = b'
  SELECT x FROM x AS x WHERE a = b

  $ ch_queries parse 'SELECT x FROM ?x WHERE a AND b'
  SELECT x FROM x AS x WHERE a AND b

  $ ch_queries parse 'SELECT x FROM ?x WHERE a OR b'
  SELECT x FROM x AS x WHERE a OR b

  $ ch_queries parse 'SELECT a + b FROM ?x'
  SELECT a + b FROM x AS x

  $ ch_queries parse 'SELECT a * b FROM ?x'
  SELECT a * b FROM x AS x
