open Ch_queries
open Ch_queries_syntax

module Params : sig
  type 'a t

  val none : unit t
  val param : string -> (_, _, 'a) Parse.t -> 'a t
  val param_with_default : string -> (_, _, 'a) Parse.t -> 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val eval : 'a t -> (Syntax.id * Syntax.expr) list -> 'a
end = struct
  type 'a t =
    | P : string * (_, _, 'a) Parse.t -> 'a t
    | P_default : string * (_, _, 'a) Parse.t * 'a -> 'a t
    | P_both : 'a t * 'b t -> ('a * 'b) t
    | P_map : ('a -> 'b) * 'a t -> 'b t
    | P_return : 'a -> 'a t

  let param name typ = P (name, typ)
  let param_with_default name typ default = P_default (name, typ, default)
  let none = P_return ()
  let map f param = P_map (f, param)
  let both p1 p2 = P_both (p1, p2)
  let ( let+ ) p f = map f p
  let ( and+ ) = both

  let rec find_arg_opt name (args : (Syntax.id * Syntax.expr) list) =
    match args with
    | [] -> None
    | (id, expr) :: rest ->
        if String.equal id.Syntax.node name then Some expr
        else find_arg_opt name rest

  let rec eval : type p. p t -> (Syntax.id * Syntax.expr) list -> p =
   fun params args ->
    match params with
    | P (name, typ) -> (
        match find_arg_opt name args with
        | Some expr -> Parse.parse_syntax typ expr
        | None ->
            Parse.parse_error (Printf.sprintf "missing parameter: %s" name))
    | P_default (name, typ, default) -> (
        match find_arg_opt name args with
        | Some expr -> Parse.parse_syntax typ expr
        | None -> default)
    | P_both (p1, p2) ->
        let v1 = eval p1 args in
        let v2 = eval p2 args in
        (v1, v2)
    | P_map (f, p) ->
        let v = eval p args in
        f v
    | P_return v -> v
end

type ('params, 'scope) a_column =
  | A_column :
      ('n, 's, 't) Parse.t * ('params -> 'scope -> ('n, 's) expr)
      -> ('params, 'scope) a_column

let a_column typ f = A_column (typ, f)

type view =
  | View : {
      db : string;
      name : string;
      params : 'p Params.t;
      from : 'p -> 'scope from;
      select : (string * ('p, 'scope) a_column) list;
      prewhere : ('p -> 'scope -> (_, bool) expr) option;
      where : ('p -> 'scope -> (_, bool) expr) option;
      group_by : ('p -> 'scope -> a_expr list) option;
      having : ('p -> 'scope -> (_, bool) expr) option;
    }
      -> view

type catalog = { mutable views : view list }

let create_catalog () = { views = [] }

let register_view (type p) catalog ~db ~name ~(params : p Params.t) ~from
    ~select ?prewhere ?where ?group_by ?having () =
  let view =
    View { db; name; params; from; select; prewhere; where; group_by; having }
  in
  catalog.views <- view :: catalog.views

let find_view catalog ~db ~name =
  List.find_opt catalog.views ~f:(fun (View v) ->
      String.equal v.db db && String.equal v.name name)

let merge_exprs existing new_ =
  match (existing, new_) with
  | None, None -> None
  | Some e, None | None, Some e -> Some e
  | Some e1, Some e2 ->
      Some (Syntax.make_expr (E_call (Func (Syntax.make_id "AND"), [ e1; e2 ])))

let rec collect_all_cols expr acc =
  match expr.Syntax.node with
  | Syntax.E_col (ns, col) -> (ns.Syntax.node, col.Syntax.node) :: acc
  | Syntax.E_call (_, args) ->
      List.fold_left args ~init:acc ~f:(fun acc e -> collect_all_cols e acc)
  | Syntax.E_in (e, _) -> collect_all_cols e acc
  | Syntax.E_lambda (_, body) -> collect_all_cols body acc
  | Syntax.E_ascribe (e, _) -> collect_all_cols e acc
  | Syntax.E_window (_, args, _) ->
      List.fold_left args ~init:acc ~f:(fun acc e -> collect_all_cols e acc)
  | _ -> acc

let collect_cols_from_select sel acc =
  match sel with
  | Syntax.Select_fields fields ->
      List.fold_left fields ~init:acc ~f:(fun acc f ->
          collect_all_cols f.Syntax.expr acc)
  | Syntax.Select_splice _ -> acc

let collect_cols_from_expr_opt e_opt acc =
  match e_opt with None -> acc | Some e -> collect_all_cols e acc

let collect_cols_from_dimensions dims acc =
  List.fold_left dims ~init:acc ~f:(fun acc dim ->
      match dim with
      | Syntax.Dimension_expr e -> collect_all_cols e acc
      | Syntax.Dimension_splice _ -> acc)

let collect_cols_from_order_by items acc =
  List.fold_left items ~init:acc ~f:(fun acc item ->
      match item with
      | Syntax.Order_by_expr (e, _, _) -> collect_all_cols e acc
      | Syntax.Order_by_splice _ -> acc)

type rewrite_info = {
  alias : string;
  from : Syntax.from;
  prewhere : Syntax.expr option;
  where : Syntax.expr option;
  group_by : Syntax.dimension list option;
  having : Syntax.expr option;
  select : string -> Syntax.expr;
}

let expand_view (View view) ~alias ~args =
  let decoded = Params.eval view.params args in
  let alias_str = alias.Syntax.node in
  (* Memoize the from thunk so scope_of_from and from_to_syntax share the same
     instance. This ensures inner subquery fields materialized via the scope
     are visible when converting to syntax. *)
  let from = memo_from (view.from decoded) in
  let scope = scope_of_from from in
  (* Evaluate columns before converting from to syntax, so that inner subquery
     fields get materialized first. *)
  let columns =
    List.map view.select ~f:(fun (name, A_column (_, f)) ->
        (name, expr_to_syntax (f decoded scope)))
  in
  let prewhere =
    Option.map (fun f -> expr_to_syntax (f decoded scope)) view.prewhere
  in
  let where =
    Option.map (fun f -> expr_to_syntax (f decoded scope)) view.where
  in
  let group_by =
    Option.map
      (fun f ->
        List.map (f decoded scope) ~f:(fun (A_expr e) ->
            Syntax.Dimension_expr (expr_to_syntax e)))
      view.group_by
  in
  let having =
    Option.map (fun f -> expr_to_syntax (f decoded scope)) view.having
  in
  let from = from_to_syntax from in
  let select col_name =
    match List.assoc_opt ~eq:String.equal col_name columns with
    | Some expr -> expr
    | None -> failwith (Printf.sprintf "unknown column: %s" col_name)
  in
  { alias = alias_str; from; prewhere; where; group_by; having; select }

let expand_view_as_subquery view ~outer_alias ~inner_alias ~args ~col_refs =
  let ri = expand_view view ~alias:inner_alias ~args in
  let col_names =
    List.filter_map col_refs ~f:(fun (a, c) ->
        if String.equal a outer_alias.Syntax.node then Some c else None)
    |> List.sort_uniq ~cmp:String.compare
  in
  let fields =
    List.map col_names ~f:(fun col_name ->
        {
          Syntax.expr = ri.select col_name;
          alias = Some (Syntax.make_id col_name);
        })
  in
  let subquery =
    Syntax.make_query
      (Q_select
         {
           with_fields = [];
           select = Select_fields fields;
           from = ri.from;
           prewhere = ri.prewhere;
           where = ri.where;
           qualify = None;
           group_by = ri.group_by;
           having = ri.having;
           order_by = None;
           limit = None;
           offset = None;
           settings = [];
         })
  in
  Syntax.make_from_one
    (F_select { select = subquery; alias = outer_alias; cluster_name = None })

type from_one_match = {
  view : view;
  alias : Syntax.id;
  inner_alias : Syntax.id;
  args : (Syntax.id * Syntax.expr) list;
}

let match_from_one catalog from_one =
  match from_one.Syntax.node with
  | Syntax.F_call { db; table; args; alias } -> (
      match find_view catalog ~db:db.node ~name:table.node with
      | None ->
          failwith (Printf.sprintf "unknown view: %s.%s" db.node table.node)
      | Some view -> Some { view; alias; inner_alias = table; args })
  | Syntax.F_table { db; table; alias; _ } -> (
      match find_view catalog ~db:db.node ~name:table.node with
      | None -> None
      | Some view -> Some { view; alias; inner_alias = table; args = [] })
  | _ -> None

let rec rewrite_expr_cols alias select expr =
  match expr.Syntax.node with
  | Syntax.E_col (ns, col) when String.equal ns.Syntax.node alias ->
      select col.Syntax.node
  | Syntax.E_call (func, args) ->
      let args' = List.map args ~f:(rewrite_expr_cols alias select) in
      Syntax.make_expr (E_call (func, args'))
  | Syntax.E_in (e, in_q) ->
      let e' = rewrite_expr_cols alias select e in
      Syntax.make_expr (E_in (e', in_q))
  | Syntax.E_lambda (params, body) ->
      let body' = rewrite_expr_cols alias select body in
      Syntax.make_expr (E_lambda (params, body'))
  | Syntax.E_ascribe (e, t) ->
      let e' = rewrite_expr_cols alias select e in
      Syntax.make_expr (E_ascribe (e', t))
  | Syntax.E_window (name, args, spec) ->
      let args' = List.map args ~f:(rewrite_expr_cols alias select) in
      Syntax.make_expr (E_window (name, args', spec))
  | _ -> expr

let rewrite_field alias select (field : Syntax.field) =
  { field with expr = rewrite_expr_cols alias select field.expr }

let rewrite_select alias select (sel : Syntax.select) =
  match sel with
  | Select_fields fields ->
      Syntax.Select_fields (List.map fields ~f:(rewrite_field alias select))
  | Select_splice _ -> sel

let rewrite_order_by alias select items =
  List.map items ~f:(function
    | Syntax.Order_by_expr (e, dir, fill) ->
        Syntax.Order_by_expr (rewrite_expr_cols alias select e, dir, fill)
    | Syntax.Order_by_splice _ as s -> s)

let rewrite_dimension alias select = function
  | Syntax.Dimension_expr e ->
      Syntax.Dimension_expr (rewrite_expr_cols alias select e)
  | Syntax.Dimension_splice _ as s -> s

let apply_rewrites_to_expr rewrites expr =
  List.fold_left rewrites ~init:expr ~f:(fun e (ri : rewrite_info) ->
      rewrite_expr_cols ri.alias ri.select e)

let apply_rewrites_to_expr_opt rewrites = function
  | None -> None
  | Some e -> Some (apply_rewrites_to_expr rewrites e)

let rewrite_join_one catalog ~col_refs join =
  match join.Syntax.node with
  | Syntax.F_call { db; table; args; alias } -> (
      match find_view catalog ~db:db.node ~name:table.node with
      | None ->
          failwith (Printf.sprintf "unknown view: %s.%s" db.node table.node)
      | Some view ->
          ( expand_view_as_subquery view ~outer_alias:alias ~inner_alias:table
              ~args ~col_refs,
            [] ))
  | Syntax.F_table { db; table; alias; _ } -> (
      match find_view catalog ~db:db.node ~name:table.node with
      | None -> (join, [])
      | Some view ->
          ( expand_view_as_subquery view ~outer_alias:alias ~inner_alias:table
              ~args:[] ~col_refs,
            [] ))
  | _ -> (join, [])

let rec rewrite_from catalog ~has_group_by ~outer_cols from =
  match from.Syntax.node with
  | Syntax.F from_one -> (
      match match_from_one catalog from_one with
      | None -> (from, [])
      | Some m ->
          let ri = expand_view m.view ~alias:m.alias ~args:m.args in
          if has_group_by && Option.is_some ri.group_by then
            let subquery_from_one =
              expand_view_as_subquery m.view ~outer_alias:m.alias
                ~inner_alias:m.inner_alias ~args:m.args ~col_refs:outer_cols
            in
            (Syntax.make_from (F subquery_from_one), [])
          else (ri.from, [ ri ]))
  | Syntax.F_join { kind; from = left; join; on } ->
      let left', left_rewrites =
        rewrite_from catalog ~has_group_by ~outer_cols left
      in
      let on_cols = collect_all_cols on [] in
      let col_refs = outer_cols @ on_cols in
      let join', join_rewrites = rewrite_join_one catalog ~col_refs join in
      let all_rewrites = left_rewrites @ join_rewrites in
      let on' = apply_rewrites_to_expr all_rewrites on in
      let from' =
        Syntax.make_from (F_join { kind; from = left'; join = join'; on = on' })
      in
      (from', all_rewrites)

and rewrite_from_one_subqueries catalog from_one =
  match from_one.Syntax.node with
  | Syntax.F_select { select; alias; cluster_name } ->
      let select' = rewrite_query catalog select in
      Syntax.make_from_one (F_select { select = select'; alias; cluster_name })
  | _ -> from_one

and rewrite_query catalog query =
  match query.Syntax.node with
  | Syntax.Q_select q ->
      let outer_cols =
        let acc = [] in
        let acc = collect_cols_from_select q.select acc in
        let acc = collect_cols_from_expr_opt q.where acc in
        let acc = collect_cols_from_expr_opt q.prewhere acc in
        let acc = collect_cols_from_expr_opt q.qualify acc in
        let acc = collect_cols_from_expr_opt q.having acc in
        let acc =
          match q.group_by with
          | None -> acc
          | Some dims -> collect_cols_from_dimensions dims acc
        in
        let acc =
          match q.order_by with
          | None -> acc
          | Some items -> collect_cols_from_order_by items acc
        in
        acc
      in
      let has_group_by = Option.is_some q.group_by in
      let from', rewrites =
        rewrite_from catalog ~has_group_by ~outer_cols q.from
      in
      let extra_prewhere =
        List.fold_left rewrites ~init:None ~f:(fun acc (ri : rewrite_info) ->
            merge_exprs acc ri.prewhere)
      in
      let extra_where =
        List.fold_left rewrites ~init:None ~f:(fun acc (ri : rewrite_info) ->
            merge_exprs acc ri.where)
      in
      let extra_group_by =
        List.fold_left rewrites ~init:[] ~f:(fun acc (ri : rewrite_info) ->
            match ri.group_by with None -> acc | Some dims -> acc @ dims)
      in
      let extra_having =
        List.fold_left rewrites ~init:None ~f:(fun acc (ri : rewrite_info) ->
            merge_exprs acc ri.having)
      in
      let select' =
        List.fold_left rewrites ~init:q.select
          ~f:(fun sel (ri : rewrite_info) ->
            rewrite_select ri.alias ri.select sel)
      in
      let where' = apply_rewrites_to_expr_opt rewrites q.where in
      let prewhere' = apply_rewrites_to_expr_opt rewrites q.prewhere in
      let group_by' =
        let existing =
          match q.group_by with
          | None -> []
          | Some dims ->
              List.fold_left rewrites ~init:dims
                ~f:(fun dims (ri : rewrite_info) ->
                  List.map dims ~f:(rewrite_dimension ri.alias ri.select))
        in
        match existing @ extra_group_by with [] -> None | dims -> Some dims
      in
      let having' =
        let existing = apply_rewrites_to_expr_opt rewrites q.having in
        merge_exprs existing extra_having
      in
      let order_by' =
        match q.order_by with
        | None -> None
        | Some items ->
            Some
              (List.fold_left rewrites ~init:items
                 ~f:(fun items (ri : rewrite_info) ->
                   rewrite_order_by ri.alias ri.select items))
      in
      let from' = rewrite_from_subqueries catalog from' in
      let prewhere_merged = merge_exprs prewhere' extra_prewhere in
      let where_merged = merge_exprs where' extra_where in
      Syntax.make_query
        (Q_select
           {
             q with
             from = from';
             select = select';
             prewhere = prewhere_merged;
             where = where_merged;
             group_by = group_by';
             having = having';
             order_by = order_by';
           })
  | Syntax.Q_union (a, b) ->
      Syntax.make_query
        (Q_union (rewrite_query catalog a, rewrite_query catalog b))
  | Syntax.Q_param _ -> query
  | Syntax.Q_ascribe (q, t) ->
      Syntax.make_query (Q_ascribe (rewrite_query catalog q, t))

and rewrite_from_subqueries catalog from =
  match from.Syntax.node with
  | Syntax.F from_one ->
      let from_one' = rewrite_from_one_subqueries catalog from_one in
      Syntax.make_from (F from_one')
  | Syntax.F_join { kind; from = left; join; on } ->
      let left' = rewrite_from_subqueries catalog left in
      let join' = rewrite_from_one_subqueries catalog join in
      Syntax.make_from (F_join { kind; from = left'; join = join'; on })

let rewrite_query_to_string catalog query_string =
  let lexbuf = Lexing.from_string query_string in
  let query = Parser.a_query (Lexer.token ()) lexbuf in
  let rewritten = rewrite_query catalog query in
  Printer.print_query rewritten
