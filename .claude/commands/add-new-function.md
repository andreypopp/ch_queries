Run `./functions-to-add.sh` to get a description of a new function to add.

You need to add it to `Ch_queries.Expr` submodule in
`ch_queries/ch_queries.ml`. Don't forget to update `ch_queries/ch_queries.mli`
as well.

For a function, you need to add a new test case `test/test_function_<name>.t`:

    Testing <function_name>:

      $ ./compile_and_run "
      > let e = {%e|<function_name>(<args>)|};;
      > #show e;;
      > print_endline (Ch_queries.expr_to_string e);;
      " --run-only

then test it `dune test test/test_function_<name>.t`
and then inspect the output
and finally promote `dune test test/test_function_<name>.t --auto-promote`

If function recives a var arg, then you need to make it accept a list of arguments, but then you'd need to special case it during staging i `ch_queries_ppx/ch_queries_ppx.ml`, see example with greatest/least functions there:
```ocaml
      | Func { node = ("greatest" | "least" | "concat") as name; _ } -> (* special case *)
```

If function has an optional argument then we need to add it as optional ocaml argument:
```ocaml
let <function_name> ?(opt_arg=default_value) arg1 arg2 = ...
```
and then add a special case staging in ppx (see above) to handle that. Also add
a test for the case with and without the optional argument.
