Test Parse.bool handles boolean JSON values:

  $ ./compile_and_run '
  > let () =
  >   let parse = Ch_queries.Parse.(parse bool) in
  >   assert (parse (`Bool true) = true);
  >   assert (parse (`Bool false) = false);
  >   print_endline "bool parsing works"
  > ' --run-only
  >>> RUNNING
  bool parsing works

Test Parse.bool handles integer 0 and 1 as booleans:

  $ ./compile_and_run '
  > let () =
  >   let parse = Ch_queries.Parse.(parse bool) in
  >   assert (parse (`Int 0) = false);
  >   assert (parse (`Int 1) = true);
  >   print_endline "int 0/1 as bool parsing works"
  > ' --run-only
  >>> RUNNING
  int 0/1 as bool parsing works

Test Parse.int handles stringified integers:

  $ ./compile_and_run '
  > let () =
  >   let parse = Ch_queries.Parse.(parse int) in
  >   assert (parse (`String "42") = 42);
  >   assert (parse (`Int 32) = 32);
  >   print_endline "stringified int parsing works"
  > ' --run-only
  >>> RUNNING
  int 0/1 as bool parsing works
