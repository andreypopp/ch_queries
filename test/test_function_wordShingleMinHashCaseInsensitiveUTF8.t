Testing wordShingleMinHashCaseInsensitiveUTF8 with single argument:

  $ ./compile_and_run "
  > let e = {%e|wordShingleMinHashCaseInsensitiveUTF8('Hello World is a test string')|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     ((Ch_queries.non_null, Ch_queries.uint64 Ch_queries.number)
      Ch_queries.tuple2,
      (Ch_queries.non_null, Ch_queries.uint64 Ch_queries.number)
      Ch_queries.tuple2)
     Ch_queries.tuple2)
    Ch_queries.expr
  wordShingleMinHashCaseInsensitiveUTF8('Hello World is a test string')

Testing wordShingleMinHashCaseInsensitiveUTF8 with shinglesize:

  $ ./compile_and_run "
  > let e = {%e|wordShingleMinHashCaseInsensitiveUTF8('Hello World is a test string', 4)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     ((Ch_queries.non_null, Ch_queries.uint64 Ch_queries.number)
      Ch_queries.tuple2,
      (Ch_queries.non_null, Ch_queries.uint64 Ch_queries.number)
      Ch_queries.tuple2)
     Ch_queries.tuple2)
    Ch_queries.expr
  wordShingleMinHashCaseInsensitiveUTF8('Hello World is a test string', 4)

Testing wordShingleMinHashCaseInsensitiveUTF8 with shinglesize and hashnum:

  $ ./compile_and_run "
  > let e = {%e|wordShingleMinHashCaseInsensitiveUTF8('Hello World is a test string', 4, 3)|};;
  > #show e;;
  > print_endline (Ch_queries.expr_to_string e);;
  > " --run-only
  >>> RUNNING
  val e :
    (Ch_queries.non_null,
     ((Ch_queries.non_null, Ch_queries.uint64 Ch_queries.number)
      Ch_queries.tuple2,
      (Ch_queries.non_null, Ch_queries.uint64 Ch_queries.number)
      Ch_queries.tuple2)
     Ch_queries.tuple2)
    Ch_queries.expr
  wordShingleMinHashCaseInsensitiveUTF8('Hello World is a test string', 4, 3)

