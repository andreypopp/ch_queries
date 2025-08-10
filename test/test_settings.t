SETTINGS with literal values:
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x FROM public.users SETTINGS max_threads=4, use_cache='"'"'true'"'"'"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun users -> Ch_queries.Row.string [%e "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:(Ch_queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~settings:
        (List.concat
           [ [ ("max_threads", `Int 4) ]; [ ("use_cache", `String "true") ] ])
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun users ->
    Ch_queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users.x AS _1
    FROM public.users AS users
    SETTINGS max_threads=4, use_cache='true') AS q

SETTINGS with parameter:
  $ ./compile_and_run '
  > let users ~max_threads = [%q "SELECT users.x FROM public.users SETTINGS max_threads=?max_threads"];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users ~max_threads =
    Ch_queries.select ()
      ~from:(Ch_queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~settings:(List.concat [ [ ("max_threads", max_threads) ] ])
  >>> RUNNING
  val users :
    max_threads:[ `Bool of bool | `Int of int | `String of string ] ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

SETTINGS with splice:
  $ ./compile_and_run '
  > let users ~settings = [%q "SELECT users.x FROM public.users SETTINGS ?settings..."];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users ~settings =
    Ch_queries.select ()
      ~from:(Ch_queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~settings:(List.concat [ settings ])
  >>> RUNNING
  val users :
    settings:(string * [ `Bool of bool | `Int of int | `String of string ])
             list ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

SETTINGS with mixed literal and splice:
  $ ./compile_and_run '
  > let users ~extra_settings = [%q "SELECT users.x FROM public.users SETTINGS max_threads=4, ?extra_settings..., use_cache='"'"'false'"'"'"];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users ~extra_settings =
    Ch_queries.select ()
      ~from:(Ch_queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~settings:
        (List.concat
           [
             [ ("max_threads", `Int 4) ];
             extra_settings;
             [ ("use_cache", `String "false") ];
           ])
  >>> RUNNING
  val users :
    extra_settings:(string *
                    [ `Bool of bool | `Int of int | `String of string ])
                   list ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

SETTINGS with multiple splices:
  $ ./compile_and_run '
  > let users ~perf_settings ~cache_settings = [%q "SELECT users.x FROM public.users SETTINGS ?perf_settings..., timeout=60, ?cache_settings..."];;
  > #show users;;
  > '
  >>> PREPROCESSING
  let users ~perf_settings ~cache_settings =
    Ch_queries.select ()
      ~from:(Ch_queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~settings:
        (List.concat [ perf_settings; [ ("timeout", `Int 60) ]; cache_settings ])
  >>> RUNNING
  val users :
    perf_settings:(string * [ `Bool of bool | `Int of int | `String of string ])
                  list ->
    cache_settings:(string *
                    [ `Bool of bool | `Int of int | `String of string ])
                   list ->
    < x : (Ch_queries.non_null, string) Ch_queries.expr > Ch_queries.scope
    Ch_queries.select

SETTINGS with boolean values (rendered as ints):
  $ ./compile_and_run '
  > let users = [%q "SELECT users.x FROM public.users SETTINGS enable_analyzer=false, use_cache=true"];;
  > let sql, _parse_row = Ch_queries.query users @@ fun users -> Ch_queries.Row.string [%e "users.x"]
  > let () = print_endline sql;;
  > '
  >>> PREPROCESSING
  let users =
    Ch_queries.select ()
      ~from:(Ch_queries.from (Database.Public.users ~alias:"users" ~final:false))
      ~select:(fun (users : _ Ch_queries.scope) ->
        object
          method x = users#query (fun users -> users#x)
        end)
      ~settings:
        (List.concat
           [ [ ("enable_analyzer", `Bool false) ]; [ ("use_cache", `Bool true) ] ])
  
  let sql, _parse_row =
    Ch_queries.query users @@ fun users ->
    Ch_queries.Row.string (users#query (fun users -> users#x))
  
  let () = print_endline sql
  >>> RUNNING
  SELECT q._1
  FROM (
    SELECT users.x AS _1
    FROM public.users AS users
    SETTINGS enable_analyzer=0, use_cache=1) AS q
