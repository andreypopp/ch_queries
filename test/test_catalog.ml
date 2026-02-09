(** This file contains view definitions to test `ch_queries_catalog` libraries,
    see `./test_catalog.t` for actual test cases with assertions. *)

open Ch_queries
open Ch_queries_catalog

module Ch_database = struct
  module Backlinks = struct
    type backlinks =
      < src_domain : (non_null, string) expr
      ; dst_domain : (non_null, string) expr
      ; user_id : (non_null, int number) expr >

    let backlinks : final:bool -> alias:string -> backlinks scope from_one =
      from_table ~db:"backlinks" ~table:"backlinks" (fun ~alias ->
          object
            method src_domain : (non_null, string) expr =
              unsafe_col alias "src_domain"

            method dst_domain : (non_null, string) expr =
              unsafe_col alias "dst_domain"

            method user_id : (non_null, int number) expr =
              unsafe_col alias "user_id"
          end)
  end

  module Public = struct
    type users =
      < id : (non_null, int number) expr ; name : (non_null, string) expr >

    let users : final:bool -> alias:string -> users scope from_one =
      from_table ~db:"public" ~table:"users" (fun ~alias ->
          object
            method id : (non_null, int number) expr = unsafe_col alias "id"
            method name : (non_null, string) expr = unsafe_col alias "name"
          end)
  end
end

let catalog = create_catalog ()

let () =
  register_view catalog ~db:"views" ~name:"backlinks"
    ~params:Params.(param "dst_domain" Parse.string)
    ~from:(fun _dst_domain ->
      {%from|FROM backlinks.backlinks AS backlinks FINAL|})
    ~select:
      [
        ( "src_domain",
          a_column Parse.string (fun _dst_domain {%s|backlinks ...|} ->
              {%e|backlinks.src_domain|}) );
        ( "dst_domain",
          a_column Parse.string (fun _dst_domain {%s|backlinks ...|} ->
              {%e|backlinks.dst_domain|}) );
        ( "user_id",
          a_column Parse.int (fun _dst_domain {%s|backlinks ...|} ->
              {%e|backlinks.user_id|}) );
      ]
    ~prewhere:(fun dst_domain {%s|backlinks ...|} ->
      let dst_domain = string dst_domain in
      {%e|backlinks.dst_domain = $dst_domain|})
    ()

let () =
  register_view catalog ~db:"views" ~name:"users" ~params:Params.none
    ~from:(fun () -> {%from|FROM public.users AS users FINAL|})
    ~select:
      [
        ("id", a_column Parse.int (fun () {%s|users ...|} -> {%e|users.id|}));
        ( "name",
          a_column Parse.string (fun () {%s|users ...|} -> {%e|users.name|}) );
      ]
    ()

let () =
  register_view catalog ~db:"views" ~name:"user_backlinks"
    ~params:Params.(param "dst_domain" Parse.string)
    ~from:(fun _dst_domain ->
      {%from|FROM public.users AS ub_u
             JOIN backlinks.backlinks AS ub_bl
             ON ub_u.id = ub_bl.user_id|})
    ~select:
      [
        ( "user_name",
          a_column Parse.string
            (fun _dst_domain {%s|ub_u ..., ub_bl ..., ...|} -> {%e|ub_u.name|})
        );
        ( "src_domain",
          a_column Parse.string
            (fun _dst_domain {%s|ub_u ..., ub_bl ..., ...|} ->
              {%e|ub_bl.src_domain|}) );
        ( "user_id",
          a_column Parse.int (fun _dst_domain {%s|ub_u ..., ub_bl ..., ...|} ->
              {%e|ub_u.id|}) );
      ]
    ~prewhere:(fun dst_domain {%s|ub_u ..., ub_bl ..., ...|} ->
      let dst_domain = string dst_domain in
      {%e|ub_bl.dst_domain = $dst_domain|})
    ()

let () =
  register_view catalog ~db:"views" ~name:"backlink_counts"
    ~params:Params.(param "dst_domain" Parse.string)
    ~from:(fun dst_domain ->
      let dst_domain = string dst_domain in
      {%from|FROM (SELECT backlinks.src_domain, backlinks.dst_domain
                   FROM backlinks.backlinks AS backlinks FINAL
                   PREWHERE backlinks.dst_domain = $dst_domain) AS agg|})
    ~select:
      [
        ( "src_domain",
          a_column Parse.string (fun _dst_domain {%s|agg ...|} ->
              {%e|agg.src_domain|}) );
        ( "dst_domain",
          a_column Parse.string (fun _dst_domain {%s|agg ...|} ->
              {%e|agg.dst_domain|}) );
      ]
    ()

let () =
  register_view catalog ~db:"views" ~name:"backlink_stats"
    ~params:Params.(param "dst_domain" Parse.string)
    ~from:(fun _dst_domain ->
      {%from|FROM backlinks.backlinks AS backlinks FINAL|})
    ~select:
      [
        ( "src_domain",
          a_column Parse.string (fun _dst_domain {%s|backlinks ...|} ->
              {%e|backlinks.src_domain|}) );
        ( "cnt",
          a_column Parse.int64 (fun _dst_domain {%s|backlinks ...|} ->
              {%e|count(backlinks.user_id)|}) );
      ]
    ~prewhere:(fun dst_domain {%s|backlinks ...|} ->
      let dst_domain = string dst_domain in
      {%e|backlinks.dst_domain = $dst_domain|})
    ~group_by:(fun _dst_domain {%s|backlinks ...|} ->
      [ A_expr {%e|backlinks.src_domain|} ])
    ~having:(fun _dst_domain {%s|backlinks ...|} ->
      {%e|count(backlinks.user_id) > 1|})
    ()

let () =
  register_view catalog ~db:"views" ~name:"backlinks_limited"
    ~params:
      Params.(
        let+ dst_domain = param "dst_domain" Parse.string
        and+ max_rows = param_with_default "max_rows" Parse.int64 100L in
        (dst_domain, max_rows))
    ~from:(fun (_dst_domain, _max_rows) ->
      {%from|FROM backlinks.backlinks AS backlinks FINAL|})
    ~select:
      [
        ( "src_domain",
          a_column Parse.string (fun (_dst_domain, _max_rows) {%s|backlinks ...|} ->
              {%e|backlinks.src_domain|}) );
      ]
    ~prewhere:(fun (dst_domain, _max_rows) {%s|backlinks ...|} ->
      let dst_domain = string dst_domain in
      {%e|backlinks.dst_domain = $dst_domain|})
    ~where:(fun (_dst_domain, max_rows) {%s|backlinks ...|} ->
      let max_rows = int64 max_rows in
      {%e|backlinks.user_id < $max_rows|})
    ()

let () =
  let sql = Sys.argv.(1) in
  let result = rewrite_query_to_string catalog sql in
  print_endline result
