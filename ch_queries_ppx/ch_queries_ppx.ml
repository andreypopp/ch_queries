open Ppxlib
open Ast_builder.Default
open Ch_queries_syntax

let set_position lexbuf loc =
  Lexing.set_position lexbuf
    { loc.Location.loc_start with pos_cnum = loc.Location.loc_start.pos_cnum }

let raise_parse_errorf ?msg name lexbuf =
  let loc_start = Lexing.lexeme_start_p lexbuf in
  let loc_end = Lexing.lexeme_end_p lexbuf in
  let loc = { Location.loc_start; loc_end; loc_ghost = false } in
  match msg with
  | None -> Location.raise_errorf ~loc "%%%s: parse error" name
  | Some msg -> Location.raise_errorf ~loc "%%%s: parse error: %s" name msg

let parse_with parser lexer name ~loc s =
  let lexbuf = Lexing.from_string s in
  set_position lexbuf loc;
  try parser lexer lexbuf with
  | Parser.Error -> raise_parse_errorf name lexbuf
  | Lexer.Error msg -> raise_parse_errorf ~msg name lexbuf
  | Uparser.Error -> raise_parse_errorf name lexbuf
  | Ulexer.Error msg -> raise_parse_errorf ~msg name lexbuf

let parse_query ~loc s = parse_with Parser.a_query Lexer.token "q" ~loc s
let parse_expr ~loc s = parse_with Parser.a_expr Lexer.token "e" ~loc s
let parse_from ~loc s = parse_with Parser.a_from Lexer.token "f" ~loc s
let parse_typ ~loc s = parse_with Parser.a_typ Lexer.token "t" ~loc s
let parse_uexpr ~loc s = parse_with Uparser.a_uexpr Ulexer.token "eu" ~loc s

let to_location ({ loc = { start_pos; end_pos }; _ } : _ Syntax.node) : location
    =
  { loc_start = start_pos; loc_end = end_pos; loc_ghost = false }

let alias from_one =
  match from_one.Syntax.node with
  | Syntax.F_table { alias; _ } | F_select { alias; _ } | F_param { alias; _ }
    ->
      alias

type from_ctx = From_none | From

let rec from_scope_expr' from =
  let open Syntax in
  match from.node with
  | F from_one ->
      let alias = alias from_one in
      [ (alias.node, evar ~loc:(to_location alias) alias.node) ]
  | F_join { from; join; _ } ->
      let x = from_scope_expr' from in
      let y =
        let alias = alias join in
        (alias.node, evar ~loc:(to_location alias) alias.node)
      in
      y :: x

let from_scope_expr ~local_bindings from =
  let loc = to_location from in
  let make fields =
    pexp_object ~loc { pcstr_self = ppat_any ~loc; pcstr_fields = fields }
  in
  let fields =
    from_scope_expr' from
    |> List.rev_map ~f:(fun (name, expr) ->
           pcf_method ~loc
             (Located.mk ~loc name, Public, Cfk_concrete (Fresh, expr)))
  in
  match local_bindings with
  | [] -> make fields
  | local_bindings ->
      [%expr
        let __q = [%e make fields] in
        [%e make (local_bindings @ fields)]]

let from_one_scope_pattern from_one =
  match from_one.Syntax.node with
  | Syntax.F_table { alias; _ } | F_select { alias; _ } | F_param { alias; _ }
    ->
      pvar ~loc:(to_location alias) alias.node

let rec from_scope_pattern ?kind from =
  let open Syntax in
  let loc = to_location from in
  match from.node with
  | F from_one ->
      let p = from_one_scope_pattern from_one in
      [%pat? ([%p p] : _ Ch_queries.scope)]
  | F_join { from; join; kind = kind'; _ } ->
      let x = from_scope_pattern from in
      let y =
        let kind = Option.value kind ~default:kind' in
        match kind with
        | `INNER_JOIN ->
            [%pat? ([%p from_one_scope_pattern join] : _ Ch_queries.scope)]
        | `LEFT_JOIN ->
            [%pat?
              ([%p from_one_scope_pattern join] : _ Ch_queries.nullable_scope)]
        | `LEFT_JOIN_OPTIONAL ->
            [%pat?
              ([%p from_one_scope_pattern join] : _ Ch_queries.nullable_scope)]
      in
      [%pat? [%p x], [%p y]]

let make_hole ~loc expr = [%expr fun __q -> [%e expr]]

let rec stage_typ typ =
  let open Syntax in
  let loc = to_location typ in
  match typ.node with
  | T { node = "Date"; _ } ->
      ( [%type: Ch_queries.non_null],
        [%type: Ch_queries.date Ch_queries.timestamp] )
  | T { node = "DateTime"; _ } ->
      ( [%type: Ch_queries.non_null],
        [%type: Ch_queries.datetime Ch_queries.timestamp] )
  | T { node = "String"; _ } -> ([%type: Ch_queries.non_null], [%type: string])
  | T { node = "Bool"; _ } -> ([%type: Ch_queries.non_null], [%type: bool])
  | T { node = "Int8" | "UInt8" | "Int16" | "UInt16" | "Int32" | "UInt32"; _ }
    ->
      ([%type: Ch_queries.non_null], [%type: int Ch_queries.number])
  | T { node = "Int64" | "UInt64"; _ } ->
      ([%type: Ch_queries.non_null], [%type: int64 Ch_queries.number])
  | T { node = "Float32" | "Float64"; _ } ->
      ([%type: Ch_queries.non_null], [%type: float Ch_queries.number])
  | T { node = t; _ } ->
      Location.raise_errorf ~loc "unknown ClickHouse type: %s" t
  | T_app ({ node = "Nullable"; _ }, [ t ]) ->
      let _, t = stage_typ t in
      ([%type: Ch_queries.null], t)
  | T_app ({ node = "Nullable"; _ }, _) ->
      Location.raise_errorf ~loc "Nullable(..) requires exactly one argument"
  | T_app ({ node = "Array"; _ }, [ t ]) ->
      let n, t = stage_typ t in
      ([%type: Ch_queries.non_null], [%type: ([%t n], [%t t]) Ch_queries.array])
  | T_app ({ node = "Array"; _ }, _) ->
      Location.raise_errorf ~loc "Array(..) requires exactly one argument"
  | T_app ({ node = "Tuple"; _ }, [ x; y ]) ->
      let xn, xt = stage_typ x in
      let yn, yt = stage_typ y in
      ( [%type: Ch_queries.non_null],
        [%type:
          ( ([%t xn], [%t xt]) Ch_queries.typ,
            ([%t yn], [%t yt]) Ch_queries.typ )
          Ch_queries.tuple2] )
  | T_app ({ node = "Tuple"; _ }, _) ->
      Location.raise_errorf ~loc "only 2-element tuples are supported"
  | T_app ({ node = t; _ }, _) ->
      Location.raise_errorf ~loc "Unknown ClickHouse type: %s" t

let query_scope ~loc scope expr =
  let e = pexp_send ~loc [%expr __q] (Located.mk ~loc scope.Syntax.node) in
  let e' = pexp_send ~loc e (Located.mk ~loc "query") in
  [%expr [%e e'] (fun __q -> [%e expr])]

let refer_to_scope ~loc ?map scope id =
  let e = pexp_send ~loc [%expr __q] (Located.mk ~loc id.Syntax.node) in
  let e = match map with None -> e | Some f -> f e in
  query_scope ~loc scope e

let map_operator_to_expr ~loc op arity =
  match (op, arity) with
  | "OR", 2 -> [%expr Ch_queries.Expr.( || )]
  | "AND", 2 -> [%expr Ch_queries.Expr.( && )]
  | "LIKE", 2 -> [%expr Ch_queries.Expr.like]
  | "NOT", 1 -> [%expr Ch_queries.Expr.not_]
  | "-", 1 -> [%expr Ch_queries.Expr.negate]
  | ">", 2 -> [%expr Ch_queries.Expr.( > )]
  | "<", 2 -> [%expr Ch_queries.Expr.( < )]
  | ">=", 2 -> [%expr Ch_queries.Expr.( >= )]
  | "<=", 2 -> [%expr Ch_queries.Expr.( <= )]
  | "!=", 2 -> [%expr Ch_queries.Expr.( != )]
  | name, _ -> evar ~loc ("Ch_queries.Expr." ^ name)

let adjust_location_for_ocaml_expr loc =
  let open Location in
  let start_pos = loc.loc_start in
  let adjusted_start =
    { start_pos with pos_cnum = start_pos.pos_cnum + 2 (* length of '?{' *) }
  in
  { loc with loc_start = adjusted_start }

let parse_ocaml_expr ~loc ocaml_code =
  let adjusted_loc = adjust_location_for_ocaml_expr loc in
  try
    let lexbuf = Lexing.from_string ocaml_code in
    (* Set the lexbuf position to match our adjusted location *)
    lexbuf.lex_start_p <- adjusted_loc.loc_start;
    lexbuf.lex_curr_p <- adjusted_loc.loc_start;
    let parsed_expr = Ppxlib.Parse.expression lexbuf in
    (* Update the location of the parsed expression *)
    { parsed_expr with pexp_loc = adjusted_loc }
  with
  | Syntaxerr.Error _ as e ->
      (* Re-raise with adjusted location *)
      raise e
  | exn ->
      Location.raise_errorf ~loc:adjusted_loc
        "Error parsing OCaml expression: %s" (Printexc.to_string exn)

let rec stage_expr ~params ~(from : from_ctx) expr =
  let loc = to_location expr in
  match expr.node with
  | Syntax.E_ascribe (e, t) ->
      let e = stage_expr ~params ~from e in
      let n, ocaml_t = stage_typ t in
      let typed_expr_type = [%type: ([%t n], [%t ocaml_t]) Ch_queries.expr] in
      [%expr ([%e e] : [%t typed_expr_type])]
  | Syntax.E_unsafe_concat xs ->
      let xs =
        List.map xs ~f:(fun e ->
            [%expr Ch_queries.A_expr [%e stage_expr ~params ~from e]])
      in
      [%expr Ch_queries.unsafe_concat [%e elist ~loc xs]]
  | Syntax.E_unsafe id ->
      let loc = to_location id in
      [%expr Ch_queries.unsafe [%e estring ~loc id.node]]
  | Syntax.E_col (scope, id) -> refer_to_scope ~loc scope id
  | Syntax.E_query (scope, expr) ->
      query_scope ~loc scope (stage_expr ~params:[] ~from:From expr)
  | Syntax.E_id id -> (
      match CCList.mem id params ~eq:Syntax.equal_id with
      | true -> [%expr Ch_queries.unsafe [%e estring ~loc id.node]]
      | false -> pexp_send ~loc [%expr __q] (Located.mk ~loc id.Syntax.node))
  | Syntax.E_lit (L_int n) -> [%expr Ch_queries.int [%e eint ~loc n]]
  | Syntax.E_lit (L_float n) ->
      [%expr Ch_queries.float [%e efloat ~loc (string_of_float n)]]
  | Syntax.E_lit L_null -> [%expr Ch_queries.null]
  | Syntax.E_lit (L_bool b) -> [%expr Ch_queries.bool [%e ebool ~loc b]]
  | Syntax.E_lit (L_string s) -> [%expr Ch_queries.string [%e estring ~loc s]]
  | Syntax.E_lit (L_interval (n, unit)) ->
      let unit_expr =
        match unit with
        | Year -> [%expr `YEAR]
        | Month -> [%expr `MONTH]
        | Week -> [%expr `WEEK]
        | Day -> [%expr `DAY]
        | Hour -> [%expr `HOUR]
        | Minute -> [%expr `MINUTE]
        | Second -> [%expr `SECOND]
      in
      [%expr Ch_queries.interval [%e eint ~loc n] [%e unit_expr]]
  | Syntax.E_window (name, args, { partition_by; order_by }) ->
      let f =
        let loc = to_location name in
        evar ~loc ("Ch_queries.Expr." ^ name.node)
      in
      let args =
        List.map args ~f:(fun arg -> (Nolabel, stage_expr ~params ~from arg))
      in
      let args =
        match partition_by with
        | None -> args
        | Some dimensions ->
            (Labelled "partition_by", stage_dimensions ~loc ~from dimensions)
            :: args
      in
      let args =
        match order_by with
        | None -> args
        | Some order_by ->
            (Labelled "order_by", stage_order_by ~loc ~from order_by) :: args
      in
      pexp_apply ~loc f args
  | Syntax.E_call (func, args) -> (
      match func with
      | Func { node = "["; _ } ->
          let xs = elist ~loc (List.map args ~f:(stage_expr ~params ~from)) in
          [%expr Ch_queries.array [%e xs]]
      | Func ({ node = "map"; _ } as name) ->
          let args =
            let rec loop args =
              match args with
              | [] -> []
              | k :: v :: args -> (k, v) :: loop args
              | [ _ ] ->
                  Location.raise_errorf ~loc
                    "map(k, v, ...): odd number of arguments"
            in
            loop args
          in
          let f =
            let loc = to_location name in
            map_operator_to_expr ~loc name.node (List.length args)
          in
          let args =
            match args with
            | [] -> [ [%expr ()] ]
            | args ->
                List.map args ~f:(fun (k, v) ->
                    pexp_tuple ~loc
                      [ stage_expr ~params ~from k; stage_expr ~params ~from v ])
          in
          eapply ~loc f [ elist ~loc args ]
      | Func name ->
          let f =
            let loc = to_location name in
            map_operator_to_expr ~loc name.node (List.length args)
          in
          let args =
            match args with
            | [] -> [ [%expr ()] ]
            | args -> List.map args ~f:(stage_expr ~params ~from)
          in
          eapply ~loc f args
      | Func_method (scope, method_name) ->
          refer_to_scope ~loc scope method_name ~map:(fun e ->
              let staged_args = List.map args ~f:(stage_expr ~params ~from) in
              eapply ~loc e staged_args))
  | Syntax.E_param var -> (
      let e = evar ~loc var.node in
      match from with From_none -> e | From -> [%expr [%e e] __q])
  | Syntax.E_ocaml_expr ocaml_code -> parse_ocaml_expr ~loc ocaml_code
  | Syntax.E_in (expr, in_query) -> (
      let expr = stage_expr ~params ~from expr in
      match in_query with
      | Syntax.In_query query ->
          let query = stage_query query in
          [%expr Ch_queries.in_ [%e expr] (Ch_queries.In_query [%e query])]
      | Syntax.In_expr { node = E_param param; _ } ->
          (* special for [E in ?param], we don't treat it as expression *)
          let loc = to_location param in
          let param = param.node in
          let param = evar ~loc param in
          [%expr Ch_queries.in_ [%e expr] [%e param]]
      | Syntax.In_expr expr' ->
          let expr' = stage_expr ~params ~from expr' in
          [%expr Ch_queries.in_ [%e expr] (Ch_queries.In_array [%e expr'])])
  | Syntax.E_lambda (param, body) ->
      let param_name = param.node in
      let body_expr = stage_expr ~params:(param :: params) ~from body in
      [%expr
        Ch_queries.lambda [%e estring ~loc param_name] (fun x -> [%e body_expr])]

and stage_dimensions ~loc ~from dimensions =
  let body =
    List.map dimensions ~f:(function
      | Syntax.Dimension_splice id ->
          let e = Syntax.make_expr ~loc:id.loc (E_param id) in
          stage_expr ~params:[] ~from e
      | Syntax.Dimension_expr expr ->
          let expr = stage_expr ~params:[] ~from expr in
          [%expr [ Ch_queries.A_expr [%e expr] ]])
  in
  [%expr List.concat [%e elist ~loc body]]

and stage_order_by ~loc ~from order_by =
  let xs =
    List.map order_by ~f:(function
      | Syntax.Order_by_splice id ->
          let e = Syntax.make_expr ~loc:id.loc (E_param id) in
          stage_expr ~params:[] ~from e
      | Syntax.Order_by_expr (expr, dir) ->
          let expr = stage_expr ~params:[] ~from expr in
          let dir =
            match dir with `ASC -> [%expr `ASC] | `DESC -> [%expr `DESC]
          in
          [%expr [ (Ch_queries.A_expr [%e expr], [%e dir]) ]])
  in
  [%expr List.concat [%e elist ~loc xs]]

and stage_settings ~loc settings =
  let body =
    List.map settings ~f:(function
      | Syntax.Setting_item (id, value) ->
          let id_str = [%expr [%e estring ~loc id.Syntax.node]] in
          let value_expr =
            match value with
            | Syntax.Setting_lit (L_int n) -> [%expr `Int [%e eint ~loc n]]
            | Syntax.Setting_lit (L_string s) ->
                [%expr `String [%e estring ~loc s]]
            | Syntax.Setting_lit (L_float _) ->
                Location.raise_errorf ~loc "float is not supported in settings"
            | Syntax.Setting_lit (L_bool b) -> [%expr `Bool [%e ebool ~loc b]]
            | Syntax.Setting_lit L_null ->
                Location.raise_errorf ~loc "NULL is not supported in settings"
            | Syntax.Setting_lit (L_interval (_, _)) ->
                Location.raise_errorf ~loc
                  "INTERVAL is not supported in settings"
            | Syntax.Setting_param param ->
                let e = Syntax.make_expr ~loc:param.loc (E_param param) in
                stage_expr ~params:[] ~from:From_none e
          in
          [%expr [ ([%e id_str], [%e value_expr]) ]]
      | Syntax.Setting_splice id ->
          let e = Syntax.make_expr ~loc:id.loc (E_param id) in
          stage_expr ~params:[] ~from:From_none e)
  in
  [%expr List.concat [%e elist ~loc body]]

and stage_field ~from idx { Syntax.expr; alias } =
  let idx = idx + 1 in
  let loc = to_location expr in
  let name, name_of_alias =
    match alias with
    | Some name ->
        let loc = to_location name in
        (Located.mk ~loc name.node, true)
    | None ->
        let name =
          match expr.node with
          | E_id id -> id.node
          | E_col (_, id) -> id.node
          | E_param id -> id.node
          | _ -> "_" ^ string_of_int idx
        in
        (Located.mk ~loc name, false)
  in
  let expr = stage_expr ~params:[] ~from expr in
  ( pcf_method ~loc (name, Public, Cfk_concrete (Fresh, expr)),
    if name_of_alias then Some name else None )

and stage_query ({ node; _ } as q) =
  match node with
  | Q_union (q1, q2) ->
      let loc = to_location q in
      let q1 = stage_query q1 in
      let q2 = stage_query q2 in
      pexp_apply ~loc [%expr Ch_queries.union] [ (Nolabel, q1); (Nolabel, q2) ]
  | Q_param id ->
      let loc = to_location id in
      evar ~loc id.node
  | Q_select
      {
        select;
        from;
        prewhere;
        where;
        qualify;
        group_by;
        having;
        order_by;
        limit;
        offset;
        settings;
      } ->
      let loc = to_location q in
      let select, local_bindings =
        match select with
        | Syntax.Select_fields fields ->
            let fields = List.mapi fields ~f:(stage_field ~from:From) in
            let pcstr_fields =
              List.map fields ~f:(function
                | expr, None -> expr
                | _expr, Some name ->
                    let expr = pexp_send ~loc [%expr __q] name in
                    pcf_method ~loc (name, Public, Cfk_concrete (Fresh, expr)))
            in
            let select_obj =
              pexp_object ~loc { pcstr_self = ppat_any ~loc; pcstr_fields }
            in
            let local_bindings =
              List.filter_map fields ~f:(fun (m, alias) ->
                  if Option.is_some alias then Some m else None)
            in
            (make_hole ~loc select_obj, local_bindings)
        | Syntax.Select_splice id ->
            let loc = to_location id in
            (evar ~loc id.node, [])
      in
      let args =
        let from =
          [%expr
            Ch_queries.map_from_scope [%e stage_from from]
              (fun [%p from_scope_pattern from] ->
                [%e from_scope_expr ~local_bindings from])]
        in
        [ (Labelled "select", select); (Labelled "from", from) ]
      in
      let args =
        match prewhere with
        | None -> args
        | Some prewhere ->
            let loc = to_location prewhere in
            let prewhere =
              make_hole ~loc (stage_expr ~params:[] ~from:From prewhere)
            in
            (Labelled "prewhere", prewhere) :: args
      in
      let args =
        match where with
        | None -> args
        | Some where ->
            let loc = to_location where in
            let where =
              make_hole ~loc (stage_expr ~params:[] ~from:From where)
            in
            (Labelled "where", where) :: args
      in
      let args =
        match qualify with
        | None -> args
        | Some qualify ->
            let loc = to_location qualify in
            let qualify =
              make_hole ~loc (stage_expr ~params:[] ~from:From qualify)
            in
            (Labelled "qualify", qualify) :: args
      in
      let args =
        match group_by with
        | None -> args
        | Some dimensions ->
            let loc = to_location q in
            let group_by = stage_dimensions ~loc ~from:From dimensions in
            let group_by = make_hole ~loc group_by in
            (Labelled "group_by", group_by) :: args
      in
      let args =
        match having with
        | None -> args
        | Some having ->
            let loc = to_location having in
            let having =
              make_hole ~loc (stage_expr ~params:[] ~from:From having)
            in
            (Labelled "having", having) :: args
      in
      let args =
        match order_by with
        | None -> args
        | Some order_by ->
            let loc = to_location q in
            let order_by = stage_order_by ~loc ~from:From order_by in
            let order_by = make_hole ~loc order_by in
            (Labelled "order_by", order_by) :: args
      in
      let args =
        match limit with
        | None -> args
        | Some expr ->
            let limit =
              make_hole ~loc (stage_expr ~params:[] ~from:From expr)
            in
            (Labelled "limit", limit) :: args
      in
      let args =
        match offset with
        | None -> args
        | Some expr ->
            let offset =
              make_hole ~loc (stage_expr ~params:[] ~from:From expr)
            in
            (Labelled "offset", offset) :: args
      in
      let args =
        match settings with
        | [] -> args
        | settings ->
            let loc = to_location q in
            let settings_expr = stage_settings ~loc settings in
            (Labelled "settings", settings_expr) :: args
      in
      pexp_apply ~loc [%expr Ch_queries.select]
        ((Nolabel, [%expr ()]) :: List.rev args)

and stage_from from =
  let open Syntax in
  let loc = to_location from in
  match from.node with
  | F from_one -> [%expr Ch_queries.from [%e stage_from_one from_one]]
  | F_join { kind; from = base; join; on } ->
      let f =
        match kind with
        | `INNER_JOIN -> [%expr Ch_queries.join]
        | `LEFT_JOIN -> [%expr Ch_queries.left_join]
        | `LEFT_JOIN_OPTIONAL -> [%expr Ch_queries.left_join ~optional:true]
      in
      let on =
        [%expr
          fun [%p from_scope_pattern ~kind:`INNER_JOIN from] ->
            let __q = [%e from_scope_expr ~local_bindings:[] from] in
            [%e stage_expr ~params:[] ~from:From on]]
      in
      [%expr [%e f] [%e stage_from base] [%e stage_from_one join] ~on:[%e on]]

and stage_from_one from_one =
  let open Syntax in
  let loc = to_location from_one in
  match from_one.node with
  | F_table { db; table; alias; final } ->
      let qname =
        Printf.sprintf "Ch_database.%s.%s"
          (String.capitalize_ascii db.node)
          table.node
      in
      [%expr
        [%e evar ~loc qname] ~alias:[%e estring ~loc alias.node]
          ~final:[%e ebool ~loc final]]
  | F_select { select; alias; cluster_name = Some cluster_name } ->
      let select_expr = stage_query select in
      let alias_expr = estring ~loc alias.node in
      let cluster_name_expr =
        match cluster_name with
        | Cluster_name id ->
            let loc = to_location id in
            estring ~loc id.node
        | Cluster_name_param id ->
            let loc = to_location id in
            evar ~loc id.node
      in
      [%expr
        Ch_queries.from_select ~cluster_name:[%e cluster_name_expr]
          [%e select_expr] ~alias:[%e alias_expr]]
  | F_select { select; alias; cluster_name = None } ->
      let select_expr = stage_query select in
      let alias_expr = estring ~loc alias.node in
      [%expr Ch_queries.from_select [%e select_expr] ~alias:[%e alias_expr]]
  | F_param { id; alias; final } -> (
      let loc = to_location id in
      match final with
      | false ->
          [%expr [%e evar ~loc id.node] ~alias:[%e estring ~loc alias.node]]
      | true ->
          [%expr
            [%e evar ~loc id.node] ~final:true
              ~alias:[%e estring ~loc alias.node]])

let expand_query ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let query = parse_query ~loc txt in
      stage_query query
  | _ -> Location.raise_errorf "expected a string literal for the '%%q"

let expand_from ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let from = parse_from ~loc txt in
      [%expr
        Ch_queries.map_from_scope [%e stage_from from]
          (fun [%p from_scope_pattern from] ->
            [%e from_scope_expr ~local_bindings:[] from])]
  | _ -> Location.raise_errorf "expected a string literal for the '%%f"

let expand_expr ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let expr = parse_expr ~loc txt in
      stage_expr ~params:[] ~from:From_none expr
  | _ -> Location.raise_errorf "expected a string literal for the '%%e"

let expand_uexpr ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let expr = parse_uexpr ~loc txt in
      stage_expr ~params:[] ~from:From_none expr
  | _ -> Location.raise_errorf "expected a string literal for the '%%eu"

let expand_typ ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let typ = parse_typ ~loc txt in
      let n, t = stage_typ typ in
      [%type: ([%t n], [%t t]) Ch_queries.expr]
  | _ -> Location.raise_errorf "expected a string literal for [%%t ...]"

let query_extension name =
  Extension.V3.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_query

let from_extension name =
  Extension.V3.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_from

let expr_extension name =
  Extension.V3.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_expr

let typ_extension name =
  Extension.V3.declare name Extension.Context.core_type
    Ast_pattern.(single_expr_payload __)
    expand_typ

let uexpr_extension name =
  Extension.V3.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_uexpr

let rules =
  [
    Context_free.Rule.extension (query_extension "ch.q");
    Context_free.Rule.extension (from_extension "ch.f");
    Context_free.Rule.extension (expr_extension "ch.e");
    Context_free.Rule.extension (typ_extension "ch.t");
    Context_free.Rule.extension (uexpr_extension "ch.eu");
  ]

let () = Driver.register_transformation ~rules "queries_ppx"
