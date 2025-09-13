Test scope extension syntax expansion

  $ ./compile_and_run "
  > let test_scope {%scope|col String, table (subcol Int32)|} = ()
  > let test_empty {%scope||} = ()
  > let test_open {%scope|col String, ...|} = ()
  > "
  >>> PREPROCESSING
  let test_scope
      (__q :
        < col : (Ch_queries.non_null, string) Ch_queries.expr
        ; table :
            < subcol :
                (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr >
            Ch_queries.scope >) =
    ()
  
  let test_empty (__q : < >) = ()
  
  let test_open
      (__q : < col : (Ch_queries.non_null, string) Ch_queries.expr ; .. >) =
    ()
  >>> RUNNING
