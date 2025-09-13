Test scope type syntax expansion

  $ ./compile_and_run "
  > type scope = {%t|(column1 String, column2 Int32)|}
  > type with_nullable_col = {%t|(column1 Nullable(String))|}
  > type empty_scope = {%t|()|}
  > type nullable_scope = {%t|?(column1 String, column2 Int32)|}
  > type empty_nullable_scope = {%t|?()|}
  > type nested_scope = {%t|(col String, subquery (scol String))|}
  > type 'a open_scope = {%t|(col String, ...)|} as 'a
  > type 'a empty_open_scope = {%t|(...)|} as 'a
  > type 'a nullable_open_scope = {%t|?(col String, ...)|} as 'a
  > type 'a empty_nullable_open_scope = {%t|?(...)|} as 'a
  > "
  >>> PREPROCESSING
  type scope =
    < column1 : (Ch_queries.non_null, string) Ch_queries.expr
    ; column2 : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr >
    Ch_queries.scope
  
  type with_nullable_col =
    < column1 : (Ch_queries.null, string) Ch_queries.expr > Ch_queries.scope
  
  type empty_scope = < > Ch_queries.scope
  
  type nullable_scope =
    < column1 : (Ch_queries.non_null, string) Ch_queries.expr
    ; column2 : (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr >
    Ch_queries.nullable_scope
  
  type empty_nullable_scope = < > Ch_queries.nullable_scope
  
  type nested_scope =
    < col : (Ch_queries.non_null, string) Ch_queries.expr
    ; subquery :
        < scol : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope >
    Ch_queries.scope
  
  type 'a open_scope =
    < col : (Ch_queries.non_null, string) Ch_queries.expr ; .. > Ch_queries.scope
    as
    'a
  
  type 'a empty_open_scope = < .. > Ch_queries.scope as 'a
  
  type 'a nullable_open_scope =
    < col : (Ch_queries.non_null, string) Ch_queries.expr ; .. >
    Ch_queries.nullable_scope
    as
    'a
  
  type 'a empty_nullable_open_scope = < .. > Ch_queries.nullable_scope as 'a
  >>> RUNNING
