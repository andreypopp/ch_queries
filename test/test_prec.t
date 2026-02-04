Arithmetic operator precedence tests - multiplication binds tighter than addition:
  $ ch_queries parse 'SELECT a + b * c FROM $x' --print-force-parens
  SELECT (a + (b * c)) FROM x AS x

  $ ch_queries parse 'SELECT a * b + c FROM $x' --print-force-parens
  SELECT ((a * b) + c) FROM x AS x

  $ ch_queries parse 'SELECT a - b * c FROM $x' --print-force-parens
  SELECT (a - (b * c)) FROM x AS x

  $ ch_queries parse 'SELECT a * b - c FROM $x' --print-force-parens
  SELECT ((a * b) - c) FROM x AS x

Division binds tighter than addition/subtraction:
  $ ch_queries parse 'SELECT a + b / c FROM $x' --print-force-parens
  SELECT (a + (b / c)) FROM x AS x

  $ ch_queries parse 'SELECT a / b + c FROM $x' --print-force-parens
  SELECT ((a / b) + c) FROM x AS x

  $ ch_queries parse 'SELECT a - b / c FROM $x' --print-force-parens
  SELECT (a - (b / c)) FROM x AS x

  $ ch_queries parse 'SELECT a / b - c FROM $x' --print-force-parens
  SELECT ((a / b) - c) FROM x AS x

Multiplication and division have same precedence - left associative:
  $ ch_queries parse 'SELECT a * b / c FROM $x' --print-force-parens
  SELECT ((a * b) / c) FROM x AS x

  $ ch_queries parse 'SELECT a / b * c FROM $x' --print-force-parens
  SELECT ((a / b) * c) FROM x AS x

Addition and subtraction have same precedence - left associative:
  $ ch_queries parse 'SELECT a + b - c FROM $x' --print-force-parens
  SELECT ((a + b) - c) FROM x AS x

  $ ch_queries parse 'SELECT a - b + c FROM $x' --print-force-parens
  SELECT ((a - b) + c) FROM x AS x

Right associativity for subtraction and division (to avoid confusion):
  $ ch_queries parse 'SELECT a - (b - c) FROM $x' --print-force-parens
  SELECT (a - (b - c)) FROM x AS x

  $ ch_queries parse 'SELECT a / (b / c) FROM $x' --print-force-parens
  SELECT (a / (b / c)) FROM x AS x

Logical operator precedence - AND binds tighter than OR:
  $ ch_queries parse 'SELECT x FROM $x WHERE a OR b AND c' --print-force-parens
  SELECT x FROM x AS x WHERE (a OR (b AND c))

  $ ch_queries parse 'SELECT x FROM $x WHERE a AND b OR c' --print-force-parens
  SELECT x FROM x AS x WHERE ((a AND b) OR c)

Comparison operators have higher precedence than logical operators:
  $ ch_queries parse 'SELECT x FROM $x WHERE a = b AND c = d' --print-force-parens
  SELECT x FROM x AS x WHERE ((a = b) AND (c = d))

  $ ch_queries parse 'SELECT x FROM $x WHERE a = b OR c = d' --print-force-parens
  SELECT x FROM x AS x WHERE ((a = b) OR (c = d))

  $ ch_queries parse 'SELECT x FROM $x WHERE a > b AND c < d' --print-force-parens
  SELECT x FROM x AS x WHERE ((a > b) AND (c < d))

Arithmetic has higher precedence than comparison:
  $ ch_queries parse 'SELECT x FROM $x WHERE a + b = c * d' --print-force-parens
  SELECT x FROM x AS x WHERE ((a + b) = (c * d))

  $ ch_queries parse 'SELECT x FROM $x WHERE a * b > c + d' --print-force-parens
  SELECT x FROM x AS x WHERE ((a * b) > (c + d))

  $ ch_queries parse 'SELECT x FROM $x WHERE a - b < c / d' --print-force-parens
  SELECT x FROM x AS x WHERE ((a - b) < (c / d))

Complex mixed expressions:
  $ ch_queries parse 'SELECT x FROM $x WHERE a + b * c = d AND e OR f' --print-force-parens
  SELECT x FROM x AS x WHERE ((((a + (b * c)) = d) AND e) OR f)

  $ ch_queries parse 'SELECT x FROM $x WHERE a OR b AND c + d * e = f' --print-force-parens
  SELECT x FROM x AS x WHERE (a OR (b AND ((c + (d * e)) = f)))

  $ ch_queries parse 'SELECT x FROM $x WHERE a > b + c AND d * e < f OR g' --print-force-parens
  SELECT x FROM x AS x WHERE (((a > (b + c)) AND ((d * e) < f)) OR g)

Parentheses override precedence:
  $ ch_queries parse 'SELECT x FROM $x WHERE (a + b) * c' --print-force-parens
  SELECT x FROM x AS x WHERE ((a + b) * c)

  $ ch_queries parse 'SELECT x FROM $x WHERE a * (b + c)' --print-force-parens
  SELECT x FROM x AS x WHERE (a * (b + c))

  $ ch_queries parse 'SELECT x FROM $x WHERE (a OR b) AND c' --print-force-parens
  SELECT x FROM x AS x WHERE ((a OR b) AND c)

  $ ch_queries parse 'SELECT x FROM $x WHERE a AND (b OR c)' --print-force-parens
  SELECT x FROM x AS x WHERE (a AND (b OR c))

Lambda expressions (lowest precedence):
  $ ch_queries parse 'SELECT x FROM $x WHERE p -> a + b' --print-force-parens
  SELECT x FROM x AS x WHERE (p -> ((a + b)))

  $ ch_queries parse 'SELECT x FROM $x WHERE p -> a AND b' --print-force-parens
  SELECT x FROM x AS x WHERE (p -> ((a AND b)))

  $ ch_queries parse 'SELECT x FROM $x WHERE (p -> a) + b' --print-force-parens
  SELECT x FROM x AS x WHERE ((p -> (a)) + b)

Edge cases - deeply nested expressions:
  $ ch_queries parse 'SELECT a + b * c + d FROM $x' --print-force-parens
  SELECT ((a + (b * c)) + d) FROM x AS x

  $ ch_queries parse 'SELECT a * b + c * d FROM $x' --print-force-parens
  SELECT ((a * b) + (c * d)) FROM x AS x

  $ ch_queries parse 'SELECT x FROM $x WHERE a AND b OR c AND d' --print-force-parens
  SELECT x FROM x AS x WHERE ((a AND b) OR (c AND d))

  $ ch_queries parse 'SELECT x FROM $x WHERE a OR b AND c OR d' --print-force-parens
  SELECT x FROM x AS x WHERE ((a OR (b AND c)) OR d)

Test without --print-force-parens to check how we can eliminate unnecessary parentheses:
  $ ch_queries parse 'SELECT x FROM $x WHERE a = b'
  SELECT x FROM x AS x WHERE a = b

  $ ch_queries parse 'SELECT x FROM $x WHERE a AND b'
  SELECT x FROM x AS x WHERE a AND b

  $ ch_queries parse 'SELECT x FROM $x WHERE a OR b'
  SELECT x FROM x AS x WHERE a OR b

  $ ch_queries parse 'SELECT a + b FROM $x'
  SELECT a + b FROM x AS x

  $ ch_queries parse 'SELECT a * b FROM $x'
  SELECT a * b FROM x AS x

  $ ch_queries parse 'SELECT a + a * b FROM $x'
  SELECT a + a * b FROM x AS x

  $ ch_queries parse 'SELECT NOT (a != b) FROM $x'
  SELECT not(a != b) FROM x AS x

  $ ch_queries parse 'SELECT NOT unsafe{a!=b} FROM $x'
  SELECT not(a!=b) FROM x AS x
