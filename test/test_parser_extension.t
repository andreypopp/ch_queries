Test basic parser generation

  $ ./compile_and_run '
  > let p_string = [%ch.parser "String"]
  > let p_int32 = [%ch.parser "Int32"]
  > let p_int64 = [%ch.parser "Int64"]
  > let p_uint64 = [%ch.parser "UInt64"]
  > let p_bool = [%ch.parser "Bool"]
  > let p_float32 = [%ch.parser "Float32"]
  > let p_float64 = [%ch.parser "Float64"]
  > let p_date = [%ch.parser "Date"]
  > let p_datetime = [%ch.parser "DateTime"]
  > '
  >>> PREPROCESSING
  let p_string = Ch_queries.Parse.string
  let p_int32 = Ch_queries.Parse.int
  let p_int64 = Ch_queries.Parse.int64
  let p_uint64 = Ch_queries.Parse.uint64
  let p_bool = Ch_queries.Parse.bool
  let p_float32 = Ch_queries.Parse.float
  let p_float64 = Ch_queries.Parse.float
  let p_date = Ch_queries.Parse.date
  let p_datetime = Ch_queries.Parse.datetime
  >>> RUNNING

Test nullable parsers

  $ ./compile_and_run '
  > let p_nullable_string = [%ch.parser "Nullable(String)"]
  > let p_nullable_int = [%ch.parser "Nullable(Int32)"]
  > '
  >>> PREPROCESSING
  let p_nullable_string = Ch_queries.Parse.nullable Ch_queries.Parse.string
  let p_nullable_int = Ch_queries.Parse.nullable Ch_queries.Parse.int
  >>> RUNNING

Test array parsers

  $ ./compile_and_run '
  > let p_array_string = [%ch.parser "Array(String)"]
  > let p_array_nullable = [%ch.parser "Array(Nullable(Int32))"]
  > '
  >>> PREPROCESSING
  let p_array_string = Ch_queries.Parse.array Ch_queries.Parse.string
  
  let p_array_nullable =
    Ch_queries.Parse.array (Ch_queries.Parse.nullable Ch_queries.Parse.int)
  >>> RUNNING

Test map parsers

  $ ./compile_and_run '
  > let p_map = [%ch.parser "Map(String, Int64)"]
  > let p_map_nullable = [%ch.parser "Map(Nullable(String), Nullable(Int32))"]
  > '
  >>> PREPROCESSING
  let p_map = Ch_queries.Parse.map Ch_queries.Parse.string Ch_queries.Parse.int64
  
  let p_map_nullable =
    Ch_queries.Parse.map
      (Ch_queries.Parse.nullable Ch_queries.Parse.string)
      (Ch_queries.Parse.nullable Ch_queries.Parse.int)
  >>> RUNNING

Test Custom type parsers

  $ ./compile_and_run '
  > type my_type = string
  > let my_type_of_json = function `String s -> s | _ -> failwith "bad"
  > let p_custom = [%ch.parser "Custom(my_type)"]
  > '
  >>> PREPROCESSING
  type my_type = string
  
  let my_type_of_json = function `String s -> s | _ -> failwith "bad"
  
  let p_custom =
    Ch_queries.Parse.custom
      (my_type_of_json, fun _ -> failwith "Custom(T): unparsing is not supported")
  >>> RUNNING

Test error: unknown type

  $ ./compile_and_run '
  > let p = [%ch.parser "UnknownType"]
  > '
  >>> PREPROCESSING
  File "-", line 2, characters 21-32:
  Error: unknown ClickHouse type: UnknownType
  [1]

Test error: unsupported DateTime64

  $ ./compile_and_run '
  > let p = [%ch.parser "DateTime64"]
  > '
  >>> PREPROCESSING
  File "-", line 2, characters 21-31:
  Error: parsing DateTime64 is not supported
  [1]

Test error: Tuple not supported

  $ ./compile_and_run '
  > let p = [%ch.parser "Tuple(Int32, String)"]
  > '
  >>> PREPROCESSING
  File "-", line 2, characters 21-41:
  Error: parsing Tuple(..) is not supported
  [1]

Test error: scope types not allowed

  $ ./compile_and_run '
  > let p = [%ch.parser "(name String)"]
  > '
  >>> PREPROCESSING
  File "-", line 2, characters 21-34:
  Error: scope types could not be used in this context
  [1]

Test complex nested types

  $ ./compile_and_run '
  > let p_nested = [%ch.parser "Array(Map(String, Nullable(Int32)))"]
  > '
  >>> PREPROCESSING
  let p_nested =
    Ch_queries.Parse.array
      (Ch_queries.Parse.map Ch_queries.Parse.string
         (Ch_queries.Parse.nullable Ch_queries.Parse.int))
  >>> RUNNING

Test Any type

  $ ./compile_and_run '
  > let p_any = [%ch.parser "Any"]
  > '
  >>> PREPROCESSING
  let p_any = Ch_queries.Parse.any
  >>> RUNNING
