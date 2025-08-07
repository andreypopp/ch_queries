Test ClickHouse type syntax expansion

  $ ./compile_and_run '
  > (* Test basic types *)
  > type test_string = [%typ "String"]
  > type test_int32 = [%typ "Int32"] 
  > type test_uint32 = [%typ "UInt32"]
  > type test_int64 = [%typ "Int64"]
  > type test_uint64 = [%typ "UInt64"]
  > type test_float32 = [%typ "Float32"]
  > 
  > (* Test nullable types *)
  > type test_nullable_string = [%typ "Nullable(String)"]
  > type test_nullable_int32 = [%typ "Nullable(Int32)"]
  > 
  > (* Test array types *)
  > type test_array_string = [%typ "Array(String)"]
  > type test_array_nullable_string = [%typ "Array(Nullable(String))"]
  > 
  > (* Test nested types *)
  > type test_nullable_array_nullable_string = [%typ "Nullable(Array(Nullable(String)))"]
  > '
  >>> PREPROCESSING
  type test_string = (Queries.non_null, string) Queries.expr
  type test_int32 = (Queries.non_null, int Queries.number) Queries.expr
  type test_uint32 = (Queries.non_null, int Queries.number) Queries.expr
  type test_int64 = (Queries.non_null, int64 Queries.number) Queries.expr
  type test_uint64 = (Queries.non_null, int64 Queries.number) Queries.expr
  type test_float32 = (Queries.non_null, float Queries.number) Queries.expr
  type test_nullable_string = (Queries.null, string) Queries.expr
  type test_nullable_int32 = (Queries.null, int Queries.number) Queries.expr
  
  type test_array_string =
    (Queries.non_null, (Queries.non_null, string) Queries.array) Queries.expr
  
  type test_array_nullable_string =
    (Queries.non_null, (Queries.null, string) Queries.array) Queries.expr
  
  type test_nullable_array_nullable_string =
    (Queries.null, (Queries.null, string) Queries.array) Queries.expr
  >>> RUNNING
