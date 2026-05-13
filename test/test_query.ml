
#use "topfind"
#require "yojson"
#require "integers"
#require "ch_queries"
#require "ch_queries.ppx"
#use "test_queries.ml"


let users = [%q "SELECT sum(users.x)over(partition by users.x order by users.x rows between unbounded preceding and current row) AS s FROM public.users"];;
let sql, _parse_row = Ch_queries.query users @@ fun __q -> Ch_queries.Row.ignore [%e "q.s"];;
let () = print_endline sql;;

