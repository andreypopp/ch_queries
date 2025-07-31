test IN expression with subquery:
  $ ./compile_and_run '
  > let users = [%query "SELECT length(arrayFilter(x -> x = 1, users.xs)) AS x FROM public.users"];;
  > let sql, _parse_row = Queries.query users @@ fun users -> Queries.Row.int [%expr "users.x"]
  > let () = print_endline sql;;
  > '

