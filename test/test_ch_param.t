Basic CH_PARAM usage:
  $ queries parse 'SELECT {user:int} FROM users'
  SELECT {user:int} FROM users AS users

CH_PARAM with different types:
  $ queries parse 'SELECT {user_id:string} FROM users'
  SELECT {user_id:string} FROM users AS users

  $ queries parse 'SELECT {count:int} FROM users'
  SELECT {count:int} FROM users AS users

  $ queries parse 'SELECT {active:bool} FROM users'
  SELECT {active:bool} FROM users AS users

  $ queries parse 'SELECT {price:float} FROM products'
  SELECT {price:float} FROM products AS products

Multiple CH_PARAM in SELECT:
  $ queries parse 'SELECT {user_id:string}, {count:int}, {active:bool} FROM users'
  SELECT {user_id:string}, {count:int}, {active:bool} FROM users AS users

CH_PARAM in WHERE clause:
  $ queries parse 'SELECT name FROM users WHERE {active:bool}'
  SELECT name FROM users AS users WHERE {active:bool}

  $ queries parse 'SELECT name FROM users WHERE age = {user_age:int}'
  SELECT name FROM users AS users WHERE (age = {user_age:int})

CH_PARAM with complex expressions:
  $ queries parse 'SELECT name FROM users WHERE {condition1:bool} AND {condition2:bool}'
  SELECT name FROM users AS users WHERE ({condition1:bool} AND {condition2:bool})

  $ queries parse 'SELECT {field:string} + {value:int} FROM table'
  SELECT ({field:string} + {value:int}) FROM table AS table

CH_PARAM with nested braces:
  $ queries parse 'SELECT {config:Map(String, {nested:int})} FROM settings'
  SELECT {config:Map(String, {nested:int})} FROM settings AS settings

CH_PARAM in function calls:
  $ queries parse 'SELECT coalesce({field:string}, {default:string}) FROM table'
  SELECT coalesce({field:string}, {default:string}) FROM table AS table

CH_PARAM with underscores and complex names:
  $ queries parse 'SELECT {user_profile_id:bigint} FROM user_profiles'
  SELECT {user_profile_id:bigint} FROM user_profiles AS user_profiles

  $ queries parse 'SELECT {camelCaseField:string} FROM table'
  SELECT {camelCaseField:string} FROM table AS table

CH_PARAM in ORDER BY and GROUP BY:
  $ queries parse 'SELECT name FROM users ORDER BY {sort_field:string}'
  SELECT name FROM users AS users ORDER BY {sort_field:string} ASC

  $ queries parse 'SELECT name FROM users GROUP BY {group_field:string}'
  SELECT name FROM users AS users GROUP BY {group_field:string}
