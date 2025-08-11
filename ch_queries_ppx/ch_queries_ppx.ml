open ContainersLabels
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

let parse_query ~loc s =
  let lexbuf = Lexing.from_string s in
  set_position lexbuf loc;
  try Parser.a_query Lexer.token lexbuf with
  | Parser.Error -> raise_parse_errorf "q" lexbuf
  | Lexer.Error msg -> raise_parse_errorf ~msg "q" lexbuf

let parse_expr ~loc s =
  let lexbuf = Lexing.from_string s in
  set_position lexbuf loc;
  try Parser.a_expr Lexer.token lexbuf with
  | Parser.Error -> raise_parse_errorf "e" lexbuf
  | Lexer.Error msg -> raise_parse_errorf ~msg "e" lexbuf

let parse_typ ~loc s =
  let lexbuf = Lexing.from_string s in
  set_position lexbuf loc;
  try Parser.a_typ Lexer.token lexbuf with
  | Parser.Error -> raise_parse_errorf "t" lexbuf
  | Lexer.Error msg -> raise_parse_errorf "t" ~msg lexbuf

let parse_uexpr ~loc s =
  let lexbuf = Lexing.from_string s in
  set_position lexbuf loc;
  try Uparser.a_uexpr Ulexer.token lexbuf with
  | Uparser.Error -> raise_parse_errorf "eu" lexbuf
  | Ulexer.Error msg -> raise_parse_errorf "eu" ~msg lexbuf

let to_location ({ loc = { start_pos; end_pos }; _ } : _ Syntax.node) : location
    =
  { loc_start = start_pos; loc_end = end_pos; loc_ghost = false }

let non_ambigius_from from =
  let open Syntax in
  match from.node with
  | Syntax.F { node = F_table { alias; _ }; _ }
  | Syntax.F { node = F_select { alias; _ }; _ }
  | Syntax.F { node = F_param { alias; _ }; _ } ->
      Some alias
  | F_join _ -> None

let rec from_scope_expr from =
  let open Syntax in
  let loc = to_location from in
  match from.node with
  | F from_one -> from_one_scope_expr from_one
  | F_join { from; join; _ } ->
      let x = from_scope_expr from in
      let y = from_one_scope_expr join in
      [%expr [%e x], [%e y]]

and from_one_scope_expr from_one =
  let open Syntax in
  let loc = to_location from_one in
  match from_one.node with
  | F_table { alias; _ } | F_select { alias; _ } | F_param { alias; _ } ->
      evar ~loc alias.node

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

and from_one_scope_pattern from_one =
  let open Syntax in
  let loc = to_location from_one in
  match from_one.node with
  | F_table { alias; _ } | F_select { alias; _ } | F_param { alias; _ } ->
      pvar ~loc alias.node

let rec typ_to_ocaml_type ~loc typ =
  let open Syntax in
  match typ.node with
  | T id -> (
      match id.node with
      | "Date" -> ([%type: Ch_queries.non_null], [%type: Ch_queries.date])
      | "String" -> ([%type: Ch_queries.non_null], [%type: string])
      | "Bool" -> ([%type: Ch_queries.non_null], [%type: bool])
      | "Int8" -> ([%type: Ch_queries.non_null], [%type: int Ch_queries.number])
      | "UInt8" -> ([%type: Ch_queries.non_null], [%type: int Ch_queries.number])
      | "Int16" -> ([%type: Ch_queries.non_null], [%type: int Ch_queries.number])
      | "UInt16" ->
          ([%type: Ch_queries.non_null], [%type: int Ch_queries.number])
      | "Int32" -> ([%type: Ch_queries.non_null], [%type: int Ch_queries.number])
      | "UInt32" ->
          ([%type: Ch_queries.non_null], [%type: int Ch_queries.number])
      | "Int64" ->
          ([%type: Ch_queries.non_null], [%type: int64 Ch_queries.number])
      | "UInt64" ->
          ([%type: Ch_queries.non_null], [%type: int64 Ch_queries.number])
      | "Float32" ->
          ([%type: Ch_queries.non_null], [%type: float Ch_queries.number])
      | "Float64" ->
          ([%type: Ch_queries.non_null], [%type: float Ch_queries.number])
      | t ->
          let loc = to_location id in
          Location.raise_errorf ~loc "unknown ClickHouse type: %s" t)
  | T_app (id, args) -> (
      match (id.node, args) with
      | "Nullable", [ t ] ->
          let _, t = typ_to_ocaml_type ~loc t in
          ([%type: Ch_queries.null], t)
      | "Nullable", _ ->
          let loc = to_location id in
          Location.raise_errorf ~loc
            "Nullable(..) requires exactly one argument"
      | "Array", [ t ] ->
          let n, t = typ_to_ocaml_type ~loc t in
          ( [%type: Ch_queries.non_null],
            [%type: ([%t n], [%t t]) Ch_queries.array] )
      | "Array", _ ->
          let loc = to_location id in
          Location.raise_errorf ~loc "Array(..) requires exactly one argument"
      | "Tuple", [ x; y ] ->
          let xn, xt = typ_to_ocaml_type ~loc x in
          let yn, yt = typ_to_ocaml_type ~loc y in
          let loc = to_location id in
          ( [%type: Ch_queries.non_null],
            [%type:
              ( ([%t xn], [%t xt]) Ch_queries.typ,
                ([%t yn], [%t yt]) Ch_queries.typ )
              Ch_queries.tuple2] )
      | "Tuple", _ ->
          let loc = to_location id in
          Location.raise_errorf ~loc "only 2-element tuples are supported"
      | t, _ ->
          let loc = to_location id in
          Location.raise_errorf ~loc "Unknown  ClickHouse type: %s" t)

let rec stage_expr ~params ~from expr =
  let loc = to_location expr in
  match expr.node with
  | Syntax.E_unsafe_concat xs ->
      let xs =
        List.map xs ~f:(fun e ->
            [%expr Ch_queries.A_expr [%e stage_expr ~params ~from e]])
      in
      [%expr Ch_queries.unsafe_concat [%e elist ~loc xs]]
  | Syntax.E_unsafe id ->
      let loc = to_location id in
      [%expr Ch_queries.unsafe [%e estring ~loc id.node]]
  | Syntax.E_col (scope, id) ->
      let e = evar ~loc scope.node in
      let e' = pexp_send ~loc e (Located.mk ~loc "query") in
      let p = pvar ~loc scope.node in
      [%expr
        [%e e'] (fun [%p p] -> [%e pexp_send ~loc e (Located.mk ~loc id.node)])]
  | Syntax.E_id id -> (
      match CCList.mem id params ~eq:Syntax.equal_id with
      | true -> [%expr Ch_queries.unsafe [%e estring ~loc id.node]]
      | false -> (
          match Option.bind from non_ambigius_from with
          | None ->
              Location.raise_errorf ~loc "ambiguous column reference" id.node
          | Some scope ->
              let e = evar ~loc scope.node in
              let e' = pexp_send ~loc e (Located.mk ~loc "query") in
              let p = pvar ~loc scope.node in
              [%expr
                [%e e'] (fun [%p p] ->
                    [%e pexp_send ~loc e (Located.mk ~loc id.node)])]))
  | Syntax.E_lit (L_int n) -> [%expr Ch_queries.int [%e eint ~loc n]]
  | Syntax.E_lit (L_float n) ->
      [%expr Ch_queries.float [%e efloat ~loc (string_of_float n)]]
  | Syntax.E_lit L_null -> [%expr Ch_queries.null]
  | Syntax.E_lit (L_bool b) -> [%expr Ch_queries.bool [%e ebool ~loc b]]
  | Syntax.E_lit (L_string s) -> [%expr Ch_queries.string [%e estring ~loc s]]
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
      | Func name ->
          let f =
            let loc = to_location name in
            match name.node with
            | "OR" -> [%expr Ch_queries.Expr.( || )]
            | "AND" -> [%expr Ch_queries.Expr.( && )]
            | _ -> evar ~loc ("Ch_queries.Expr." ^ name.node)
          in
          eapply ~loc f (List.map args ~f:(stage_expr ~params ~from))
      | Func_method (table, method_name) ->
          let table_loc = to_location table in
          let method_loc = to_location method_name in
          let e = evar ~loc:table_loc table.node in
          let e' =
            pexp_send ~loc:table_loc e (Located.mk ~loc:table_loc "query")
          in
          let p = pvar ~loc:table_loc table.node in
          let method_call =
            pexp_send ~loc:method_loc e
              (Located.mk ~loc:method_loc method_name.node)
          in
          let staged_args = List.map args ~f:(stage_expr ~params ~from) in
          let method_call_with_args =
            eapply ~loc:method_loc method_call staged_args
          in
          [%expr [%e e'] (fun [%p p] -> [%e method_call_with_args])])
  | Syntax.E_param (var, typ) -> (
      let e = evar ~loc var.node in
      let e_with_scope =
        match Option.map from_scope_expr from with
        | None -> e
        | Some arg -> [%expr [%e e] [%e arg]]
      in
      match typ with
      | None -> e_with_scope
      | Some t ->
          let n, ocaml_t = typ_to_ocaml_type ~loc t in
          let typed_expr_type =
            [%type: ([%t n], [%t ocaml_t]) Ch_queries.expr]
          in
          [%expr ([%e e_with_scope] : [%t typed_expr_type])])
  | Syntax.E_ocaml_expr ocaml_code -> (
      (* Parse the OCaml expression and adjust location for error reporting *)
      let adjusted_loc =
        let open Location in
        let start_pos = loc.loc_start in
        (* Offset by 2 characters to account for '?{' prefix *)
        let adjusted_start =
          { start_pos with pos_cnum = start_pos.pos_cnum + 2 }
        in
        { loc with loc_start = adjusted_start }
      in
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
            "Error parsing OCaml expression: %s" (Printexc.to_string exn))
  | Syntax.E_in (expr, in_query) -> (
      let expr = stage_expr ~params ~from expr in
      match in_query with
      | Syntax.In_query query ->
          let query = stage_query query in
          [%expr Ch_queries.in_ [%e expr] (Ch_queries.In_query [%e query])]
      | Syntax.In_expr { node = E_param (param, _typ); _ } ->
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
          let e = Syntax.make_expr ~loc:id.loc (E_param (id, None)) in
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
          let e = Syntax.make_expr ~loc:id.loc (E_param (id, None)) in
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
                failwith
                  "invariant violation: float is not supported in settings"
            | Syntax.Setting_lit (L_bool b) -> [%expr `Bool [%e ebool ~loc b]]
            | Syntax.Setting_lit L_null ->
                failwith
                  "invariant violation: NULL is not supported in settings"
            | Syntax.Setting_param param ->
                let e =
                  Syntax.make_expr ~loc:param.loc (E_param (param, None))
                in
                stage_expr ~params:[] ~from:None e
          in
          [%expr [ ([%e id_str], [%e value_expr]) ]]
      | Syntax.Setting_splice id ->
          let e = Syntax.make_expr ~loc:id.loc (E_param (id, None)) in
          stage_expr ~params:[] ~from:None e)
  in
  [%expr List.concat [%e elist ~loc body]]

and stage_field ~from idx { Syntax.expr; alias } =
  let idx = idx + 1 in
  let loc = to_location expr in
  let name =
    let name, loc =
      match alias with
      | Some name ->
          let loc = to_location name in
          (name.node, loc)
      | None -> (
          match expr.node with
          | E_id id -> (id.node, loc)
          | E_col (_, id) -> (id.node, loc)
          | E_param (id, _) -> (id.node, loc)
          | _ -> ("_" ^ string_of_int idx, loc))
    in
    Located.mk ~loc name
  in
  pcf_method ~loc
    (name, Public, Cfk_concrete (Fresh, stage_expr ~params:[] ~from expr))

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
      let select =
        match select with
        | Syntax.Select_fields fields ->
            let select_methods =
              List.mapi fields ~f:(stage_field ~from:(Some from))
            in
            let select_obj =
              pexp_object ~loc
                { pcstr_self = ppat_any ~loc; pcstr_fields = select_methods }
            in
            pexp_fun ~loc Nolabel None (from_scope_pattern from) select_obj
        | Syntax.Select_splice id ->
            let loc = to_location id in
            evar ~loc id.node
      in
      let args =
        [ (Labelled "select", select); (Labelled "from", stage_from from) ]
      in
      let args =
        match prewhere with
        | None -> args
        | Some prewhere ->
            let loc = to_location prewhere in
            let prewhere =
              pexp_fun ~loc Nolabel None (from_scope_pattern from)
                (stage_expr ~params:[] ~from:(Some from) prewhere)
            in
            (Labelled "prewhere", prewhere) :: args
      in
      let args =
        match where with
        | None -> args
        | Some where ->
            let loc = to_location where in
            let where =
              pexp_fun ~loc Nolabel None (from_scope_pattern from)
                (stage_expr ~params:[] ~from:(Some from) where)
            in
            (Labelled "where", where) :: args
      in
      let args =
        match qualify with
        | None -> args
        | Some qualify ->
            let loc = to_location qualify in
            let qualify =
              pexp_fun ~loc Nolabel None (from_scope_pattern from)
                (stage_expr ~params:[] ~from:(Some from) qualify)
            in
            (Labelled "qualify", qualify) :: args
      in
      let args =
        match group_by with
        | None -> args
        | Some dimensions ->
            let loc = to_location q in
            let group_by = stage_dimensions ~loc ~from:(Some from) dimensions in
            let group_by =
              pexp_fun ~loc Nolabel None (from_scope_pattern from) group_by
            in
            (Labelled "group_by", group_by) :: args
      in
      let args =
        match having with
        | None -> args
        | Some having ->
            let loc = to_location having in
            let having =
              pexp_fun ~loc Nolabel None (from_scope_pattern from)
                (stage_expr ~params:[] ~from:(Some from) having)
            in
            (Labelled "having", having) :: args
      in
      let args =
        match order_by with
        | None -> args
        | Some order_by ->
            let loc = to_location q in
            let order_by = stage_order_by ~loc ~from:(Some from) order_by in
            let order_by =
              pexp_fun ~loc Nolabel None (from_scope_pattern from) order_by
            in
            (Labelled "order_by", order_by) :: args
      in
      let args =
        match limit with
        | None -> args
        | Some expr ->
            let limit =
              pexp_fun ~loc Nolabel None (from_scope_pattern from)
                (stage_expr ~params:[] ~from:(Some from) expr)
            in
            (Labelled "limit", limit) :: args
      in
      let args =
        match offset with
        | None -> args
        | Some expr ->
            let offset =
              pexp_fun ~loc Nolabel None (from_scope_pattern from)
                (stage_expr ~params:[] ~from:(Some from) expr)
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
        pexp_fun ~loc Nolabel None
          (from_scope_pattern ~kind:`INNER_JOIN from)
          (stage_expr ~params:[] ~from:(Some from) on)
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

let expand_select ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let query = parse_query ~loc txt in
      stage_query query
  | _ -> Location.raise_errorf "expected a string literal for the '%%q"

let expand_expr ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let expr = parse_expr ~loc txt in
      stage_expr ~params:[] ~from:None expr
  | _ -> Location.raise_errorf "expected a string literal for the '%%e"

let expand_uexpr ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let expr = parse_uexpr ~loc txt in
      stage_expr ~params:[] ~from:None expr
  | _ -> Location.raise_errorf "expected a string literal for the '%%eu"

let expand_typ ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let typ = parse_typ ~loc txt in
      let n, t = typ_to_ocaml_type ~loc typ in
      [%type: ([%t n], [%t t]) Ch_queries.expr]
  | _ -> Location.raise_errorf "expected a string literal for [%%t ...]"

let select_extension =
  Extension.V3.declare "q" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_select

let expr_extension =
  Extension.V3.declare "e" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_expr

let typ_extension =
  Extension.V3.declare "t" Extension.Context.core_type
    Ast_pattern.(single_expr_payload __)
    expand_typ

let uexpr_extension =
  Extension.V3.declare "eu" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_uexpr

let rules =
  [
    Context_free.Rule.extension select_extension;
    Context_free.Rule.extension expr_extension;
    Context_free.Rule.extension typ_extension;
    Context_free.Rule.extension uexpr_extension;
  ]

let () = Driver.register_transformation ~rules "queries_ppx"
