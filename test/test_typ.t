Test ClickHouse type syntax expansion

  $ ./compile_and_run '
  > (* Test basic types *)
  > type test_string = [%t "String"]
  > type test_int32 = [%t "Int32"]
  > type test_uint32 = [%t "UInt32"]
  > type test_int64 = [%t "Int64"]
  > type test_uint64 = [%t "UInt64"]
  > type test_float32 = [%t "Float32"]
  > 
  > (* Test nullable types *)
  > type test_nullable_string = [%t "Nullable(String)"]
  > type test_nullable_int32 = [%t "Nullable(Int32)"]
  > 
  > (* Test array types *)
  > type test_array_string = [%t "Array(String)"]
  > type test_array_nullable_string = [%t "Array(Nullable(String))"]
  > 
  > (* Test map types *)
  > type test_map_string = [%t "Map(String, Int64)"]
  > type test_map_nullable_string = [%t "Map(Nullable(String), Nullable(Float32))"]
  > 
  > (* Test nested types *)
  > type test_nullable_array_nullable_string = [%t "Nullable(Array(Nullable(String)))"]
  > '
  >>> PREPROCESSING
  type test_string = (Ch_queries.non_null, string) Ch_queries.expr
  type test_int32 = (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  type test_uint32 = (Ch_queries.non_null, int Ch_queries.number) Ch_queries.expr
  type test_int64 = (Ch_queries.non_null, int64 Ch_queries.number) Ch_queries.expr
  
  type test_uint64 =
    (Ch_queries.non_null, int64 Ch_queries.number) Ch_queries.expr
  
  type test_float32 =
    (Ch_queries.non_null, float Ch_queries.number) Ch_queries.expr
  
  type test_nullable_string = (Ch_queries.null, string) Ch_queries.expr
  
  type test_nullable_int32 =
    (Ch_queries.null, int Ch_queries.number) Ch_queries.expr
  
  type test_array_string =
    ( Ch_queries.non_null,
      (Ch_queries.non_null, string) Ch_queries.array )
    Ch_queries.expr
  
  type test_array_nullable_string =
    ( Ch_queries.non_null,
      (Ch_queries.null, string) Ch_queries.array )
    Ch_queries.expr
  
  type test_map_string =
    ( Ch_queries.non_null,
      ( Ch_queries.non_null,
        string,
        Ch_queries.non_null,
        int64 Ch_queries.number )
      Ch_queries.map )
    Ch_queries.expr
  
  type test_map_nullable_string =
    ( Ch_queries.non_null,
      ( Ch_queries.null,
        string,
        Ch_queries.null,
        float Ch_queries.number )
      Ch_queries.map )
    Ch_queries.expr
  
  type test_nullable_array_nullable_string =
    (Ch_queries.null, (Ch_queries.null, string) Ch_queries.array) Ch_queries.expr
  >>> RUNNING
