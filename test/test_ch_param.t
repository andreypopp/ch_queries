Basic CH_PARAM usage:
  $ ch_queries parse 'SELECT {user:int} FROM users'
  SELECT {user:int} FROM users AS users

CH_PARAM with complex expressions:
  $ ch_queries parse 'SELECT {field:string} + {value:int} FROM table'
  SELECT ({field:string} + {value:int}) FROM table AS table

CH_PARAM with nested braces:
  $ ch_queries parse 'SELECT {config:Map(String, {nested:int})} FROM config'
  SELECT {config:Map(String, {nested:int})} FROM config AS config
