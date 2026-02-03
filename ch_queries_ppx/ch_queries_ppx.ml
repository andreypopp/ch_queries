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

let parse_with parser token name ~loc s =
  let lexbuf = Lexing.from_string s in
  set_position lexbuf loc;
  try parser (token ()) lexbuf with
  | Parser.Error -> raise_parse_errorf name lexbuf
  | Lexer.Error msg -> raise_parse_errorf ~msg name lexbuf
  | Uparser.Error -> raise_parse_errorf name lexbuf
  | Ulexer.Error msg -> raise_parse_errorf ~msg name lexbuf

let parse_query ~loc s = parse_with Parser.a_query Lexer.token "q" ~loc s
let parse_expr ~loc s = parse_with Parser.a_expr Lexer.token "e" ~loc s
let parse_from ~loc s = parse_with Parser.a_from Lexer.token "f" ~loc s
let parse_typ ~loc s = parse_with Parser.a_typ Lexer.token "t" ~loc s
let parse_uexpr ~loc s = parse_with Uparser.a_uexpr Ulexer.token "eu" ~loc s

let parse_scope_columns ~loc s =
  parse_with Parser.a_scope_columns Lexer.token "scope" ~loc s

let to_location ({ loc = { start_pos; end_pos }; _ } : _ Syntax.node) : location
    =
  { loc_start = start_pos; loc_end = end_pos; loc_ghost = false }

let id_to_located id = Located.mk ~loc:(to_location id) id.node

let rec alias from_one =
  match from_one.Syntax.node with
  | Syntax.F_table { alias; _ } | F_select { alias; _ } | F_param { alias; _ }
    ->
      alias
  | F_ascribe (f, _) -> alias f

let refer_to_db_table db table =
  Printf.sprintf "Ch_database.%s.%s"
    (String.capitalize_ascii db.Syntax.node)
    table.Syntax.node

let rec from_scope_expr' from =
  let open Syntax in
  match from.node with
  | F from_one ->
      let alias = alias from_one in
      [ (alias, evar ~loc:(to_location alias) alias.node) ]
  | F_join { from; join; _ } ->
      let x = from_scope_expr' from in
      let y =
        let alias = alias join in
        (alias, evar ~loc:(to_location alias) alias.node)
      in
      y :: x

let located_of_id id =
  let loc = to_location id in
  Located.mk ~loc id.node

module Id_set = Set.Make (struct
  type t = Syntax.id

  let compare = Syntax.compare_id
end)

let from_scope_expr ~scopes from =
  let loc = to_location from in
  let make fields =
    pexp_object ~loc
      { pcstr_self = ppat_any ~loc; pcstr_fields = List.map fields ~f:snd }
  in
  let merge_scopes x y =
    let seen = List.to_seq y |> Seq.map fst |> Id_set.of_seq in
    let x = List.filter x ~f:(fun (id, _) -> not (Id_set.mem id seen)) in
    x @ y
  in
  let from =
    from_scope_expr' from
    |> List.rev_map ~f:(fun (id, expr) ->
           ( id,
             pcf_method ~loc
               (located_of_id id, Public, Cfk_concrete (Fresh, expr)) ))
  in
  let rec build curr_scope = function
    | [] -> failwith "impossible: should have at least FROM scope"
    | x :: [] ->
        let curr_scope = merge_scopes curr_scope x in
        make curr_scope
    | x :: xs ->
        let curr_scope = merge_scopes curr_scope x in
        [%expr
          let __q = [%e make curr_scope] in
          [%e build (merge_scopes curr_scope x) xs]]
  in
  let scopes = List.filter scopes ~f:(fun scope -> not (List.is_empty scope)) in
  build [] (from :: scopes)

let rec from_one_scope_pattern from_one =
  match from_one.Syntax.node with
  | Syntax.F_table { alias; _ } | F_select { alias; _ } | F_param { alias; _ }
    ->
      pvar ~loc:(to_location alias) alias.node
  | F_ascribe (f, _) -> from_one_scope_pattern f

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

let nullable =
  let loc = Location.none in
  function
  | `NULL -> [%type: Ch_queries.null]
  | `NON_NULL -> [%type: Ch_queries.non_null]
  | `ANY -> [%type: _]

let rec stage_typ' typ =
  let open Syntax in
  let loc = to_location typ in
  match typ.node with
  | T_any -> (`ANY, [%type: _])
  | T_custom _ -> (`ANY, [%type: _]) (* treat Custom(_) as Any for casts *)
  | T { node = "Date"; _ } -> (`NON_NULL, [%type: Ch_queries.date])
  | T { node = "DateTime64"; _ } -> (`NON_NULL, [%type: Ch_queries.datetime64])
  | T { node = "DateTime"; _ } -> (`NON_NULL, [%type: Ch_queries.datetime])
  | T { node = "String"; _ } -> (`NON_NULL, [%type: string])
  | T { node = "Bool"; _ } -> (`NON_NULL, [%type: bool])
  | T { node = "Int8" | "UInt8" | "Int16" | "UInt16" | "Int32" | "UInt32"; _ }
    ->
      (`NON_NULL, [%type: int Ch_queries.number])
  | T { node = "Int64"; _ } -> (`NON_NULL, [%type: int64 Ch_queries.number])
  | T { node = "UInt64"; _ } ->
      (`NON_NULL, [%type: Unsigned.uint64 Ch_queries.number])
  | T { node = "Float32" | "Float64" | "Float"; _ } ->
      (`NON_NULL, [%type: float Ch_queries.number])
  | T { node = t; _ } ->
      Location.raise_errorf ~loc "unknown ClickHouse type: %s" t
  | T_app ({ node = "Nullable"; _ }, [ t ]) ->
      let _, t = stage_typ' t in
      (`NULL, t)
  | T_app ({ node = "Nullable"; _ }, _) ->
      Location.raise_errorf ~loc "Nullable(..) requires exactly one argument"
  | T_app ({ node = "Array"; _ }, [ t ]) ->
      let n, t = stage_typ' t in
      (`NON_NULL, [%type: ([%t nullable n], [%t t]) Ch_queries.array])
  | T_app ({ node = "Array"; _ }, _) ->
      Location.raise_errorf ~loc "Array(..) requires exactly one argument"
  | T_app ({ node = "Map"; _ }, [ k; v ]) ->
      let nk, k = stage_typ' k in
      let nv, v = stage_typ' v in
      ( `NON_NULL,
        [%type:
          ([%t nullable nk], [%t k], [%t nullable nv], [%t v]) Ch_queries.map]
      )
  | T_app ({ node = "Map"; _ }, _) ->
      Location.raise_errorf ~loc "Map(..) requires exactly two argument"
  | T_app ({ node = "Tuple"; _ }, [ x; y ]) ->
      let xn, xt = stage_typ' x in
      let yn, yt = stage_typ' y in
      ( `NON_NULL,
        [%type:
          ( ([%t nullable xn], [%t xt]) Ch_queries.typ,
            ([%t nullable yn], [%t yt]) Ch_queries.typ )
          Ch_queries.tuple2] )
  | T_app ({ node = "Tuple"; _ }, _) ->
      Location.raise_errorf ~loc "only 2-element tuples are supported"
  | T_app ({ node = t; _ }, _) ->
      Location.raise_errorf ~loc "Unknown ClickHouse type: %s" t
  | T_scope (cols, is_open, nullable) ->
      let t =
        let fields =
          List.map cols ~f:(fun { Syntax.name; typ } ->
              let typ = stage_typ typ in
              let loc = to_location name in
              let pof_desc = Otag (Located.mk ~loc name.node, typ) in
              { pof_desc; pof_attributes = []; pof_loc = loc })
        in
        let closed_flag =
          match is_open with `Open -> Open | `Closed -> Closed
        in
        ptyp_object ~loc fields closed_flag
      in
      ((nullable :> [ `NON_NULL | `NULL | `ANY ]), t)
  | T_db_table (db, table, nullable) ->
      let lid = Longident.parse (refer_to_db_table db table) in
      let lid = Located.mk ~loc lid in
      let t = ptyp_constr ~loc lid [] in
      ((nullable :> [ `NON_NULL | `NULL | `ANY ]), t)

and stage_scope_columns ~loc ~is_open cols =
  let fields =
    List.map cols ~f:(fun { Syntax.name; typ } ->
        let typ = stage_typ typ in
        let loc = to_location name in
        let pof_desc = Otag (Located.mk ~loc name.node, typ) in
        { pof_desc; pof_attributes = []; pof_loc = loc })
  in
  let closed_flag = match is_open with `Open -> Open | `Closed -> Closed in
  ptyp_object ~loc fields closed_flag

and stage_typ x =
  let loc = to_location x in
  let n, t = stage_typ' x in
  if Syntax.is_scope_typ x then
    match n with
    | `NULL -> [%type: [%t t] Ch_queries.nullable_scope]
    | `NON_NULL -> [%type: [%t t] Ch_queries.scope]
    | `ANY -> [%type: impossible_this_is_a_bug]
  else [%type: ([%t nullable n], [%t t]) Ch_queries.expr]

let rec stage_typ_to_parser typ =
  let open Syntax in
  let loc = to_location typ in
  match typ.node with
  | T_any -> [%expr Ch_queries.Parse.any]
  | T { node = "Date"; _ } -> [%expr Ch_queries.Parse.date]
  | T { node = "DateTime64"; _ } ->
      Location.raise_errorf ~loc "parsing DateTime64 is not supported"
  | T { node = "DateTime"; _ } -> [%expr Ch_queries.Parse.datetime]
  | T { node = "String"; _ } -> [%expr Ch_queries.Parse.string]
  | T { node = "Bool"; _ } -> [%expr Ch_queries.Parse.bool]
  | T { node = "Int8" | "UInt8" | "Int16" | "UInt16" | "Int32" | "UInt32"; _ }
    ->
      [%expr Ch_queries.Parse.int]
  | T { node = "Int64"; _ } -> [%expr Ch_queries.Parse.int64]
  | T { node = "UInt64"; _ } -> [%expr Ch_queries.Parse.uint64]
  | T { node = "Float32" | "Float64" | "Float"; _ } ->
      [%expr Ch_queries.Parse.float]
  | T { node = t; _ } ->
      Location.raise_errorf ~loc "unknown ClickHouse type: %s" t
  | T_custom { node = ocaml_type; _ } ->
      let of_json = evar ~loc (ocaml_type ^ "_of_json") in
      let to_sql =
        [%expr fun _ -> failwith "Custom(T): unparsing is not supported"]
      in
      [%expr Ch_queries.Parse.custom ([%e of_json], [%e to_sql])]
  | T_app ({ node = "Nullable"; _ }, [ t ]) ->
      [%expr Ch_queries.Parse.nullable [%e stage_typ_to_parser t]]
  | T_app ({ node = "Nullable"; _ }, _) ->
      Location.raise_errorf ~loc "Nullable(..) requires exactly one argument"
  | T_app ({ node = "Array"; _ }, [ t ]) ->
      [%expr Ch_queries.Parse.array [%e stage_typ_to_parser t]]
  | T_app ({ node = "Array"; _ }, _) ->
      Location.raise_errorf ~loc "Array(..) requires exactly one argument"
  | T_app ({ node = "Map"; _ }, [ k; v ]) ->
      [%expr
        Ch_queries.Parse.map [%e stage_typ_to_parser k]
          [%e stage_typ_to_parser v]]
  | T_app ({ node = "Map"; _ }, _) ->
      Location.raise_errorf ~loc "Map(..) requires exactly two argument"
  | T_app ({ node = "Tuple"; _ }, _) ->
      Location.raise_errorf ~loc "parsing Tuple(..) is not supported"
  | T_app ({ node = t; _ }, _) ->
      Location.raise_errorf ~loc "Unknown ClickHouse type: %s" t
  | T_scope _ ->
      Location.raise_errorf ~loc "scope types could not be used in this context"
  | T_db_table _ ->
      Location.raise_errorf ~loc "table types could not be used in this context"

let query_scope ~loc scope expr =
  let e = pexp_send ~loc [%expr __q] (Located.mk ~loc scope.Syntax.node) in
  let e' = pexp_send ~loc e (Located.mk ~loc "query") in
  [%expr [%e e'] (fun __q -> [%e expr])]

let normalise_col name =
  (if Char.is_uppercase_ascii name.[0] then "_" ^ name else name)
  |> String.split_on_char ~by:'.'
  |> String.concat ~sep:"__dot__"

let refer_to_scope ~loc ?map scope id =
  let e =
    pexp_send ~loc [%expr __q] (Located.mk ~loc (normalise_col id.Syntax.node))
  in
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
  | "if", 3 -> [%expr Ch_queries.Expr.(if_)]
  | name, _ -> (
      match String.chop_prefix name ~pre:"JSON" with
      | Some suf ->
          let name = "json" ^ suf in
          evar ~loc ("Ch_queries.Expr." ^ name)
      | None -> (
          match String.chop_prefix name ~pre:"YYYYMMDD" with
          | Some suf ->
              let name = "yYYYMMDD" ^ suf in
              evar ~loc ("Ch_queries.Expr." ^ name)
          | None -> evar ~loc ("Ch_queries.Expr." ^ name)))

let adjust_location_for_ocaml_expr loc =
  let open Location in
  let start_pos = loc.loc_start in
  let adjusted_start =
    { start_pos with pos_cnum = start_pos.pos_cnum + String.length "?{" }
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

let rec extract_alias expr =
  match expr.Syntax.node with
  | Syntax.E_id id -> Some id
  | E_param { param; _ } -> Some param
  | E_col (_, id) -> Some id
  | E_ascribe (expr, _) -> extract_alias expr
  | E_query (_, expr) -> extract_alias expr
  | _ -> None

(* Stage an expression for use in a clause (WHERE, HAVING, etc.) that supports
   optional params. Returns (label, staged_expr) where label is Optional if the
   expression is an immediate optional param with scope access (?$.param),
   Labelled otherwise. For optional params, we just pass the variable directly;
   OCaml's optional argument syntax handles the option unwrapping.
   For regular expressions, it's wrapped in make_hole. *)
let rec stage_clause_expr ~label expr =
  let loc = to_location expr in
  match expr.Syntax.node with
  | Syntax.E_param { param; param_has_scope; param_optional = true } ->
      if not param_has_scope then
        Location.raise_errorf ~loc:(to_location param)
          "?$%s: optional parameters must use scope access syntax (?$.%s)"
          param.Syntax.node param.Syntax.node;
      let var = evar ~loc:(to_location param) param.Syntax.node in
      (Optional label, var)
  | Syntax.E_ascribe
      ( { node = E_param { param; param_has_scope; param_optional = true }; _ },
        _ ) ->
      if not param_has_scope then
        Location.raise_errorf ~loc:(to_location param)
          "?$%s: optional parameters must use scope access syntax (?$.%s)"
          param.Syntax.node param.Syntax.node;
      let var = evar ~loc:(to_location param) param.Syntax.node in
      (Optional label, var)
  | _ -> (Labelled label, make_hole ~loc (stage_expr ~params:[] expr))

and stage_expr ~params expr =
  let loc = to_location expr in
  match expr.node with
  | Syntax.E_ascribe (e, t) ->
      let e = stage_expr ~params e in
      [%expr ([%e e] : [%t stage_typ t])]
  | Syntax.E_unsafe_concat xs ->
      let xs =
        List.map xs ~f:(fun e ->
            [%expr Ch_queries.A_expr [%e stage_expr ~params e]])
      in
      [%expr Ch_queries.unsafe_concat [%e elist ~loc xs]]
  | Syntax.E_unsafe id ->
      let loc = to_location id in
      [%expr Ch_queries.unsafe [%e estring ~loc id.node]]
  | Syntax.E_col (scope, id) -> refer_to_scope ~loc scope id
  | Syntax.E_query (scope, expr) ->
      query_scope ~loc scope (stage_expr ~params:[] expr)
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
        List.map args ~f:(fun arg -> (Nolabel, stage_expr ~params arg))
      in
      let args =
        match partition_by with
        | None -> args
        | Some dimensions ->
            (Labelled "partition_by", stage_dimensions ~loc dimensions) :: args
      in
      let args =
        match order_by with
        | None -> args
        | Some order_by ->
            (Labelled "order_by", stage_window_order_by ~loc order_by) :: args
      in
      pexp_apply ~loc f args
  | Syntax.E_call (func, args) -> (
      match func with
      | Func { node = "["; _ } ->
          let xs = elist ~loc (List.map args ~f:(stage_expr ~params)) in
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
                      [ stage_expr ~params k; stage_expr ~params v ])
          in
          eapply ~loc f [ elist ~loc args ]
      | Func ({ node = "multiIf"; _ } as name) ->
          let args, else_ =
            let rec loop args =
              match args with
              | [] ->
                  Location.raise_errorf ~loc
                    "multiIf(k, v, ...): even number of arguments, missing \
                     else branch"
              | k :: v :: args ->
                  let cases, else_ = loop args in
                  ((k, v) :: cases, else_)
              | [ else_ ] -> ([], else_)
            in
            loop args
          in
          let f =
            let loc = to_location name in
            map_operator_to_expr ~loc name.node (List.length args)
          in
          let args =
            match args with
            | [] ->
                Location.raise_errorf ~loc
                  "multiIf(...) requires at least one case"
            | args ->
                List.map args ~f:(fun (k, v) ->
                    pexp_tuple ~loc
                      [ stage_expr ~params k; stage_expr ~params v ])
          in
          pexp_apply ~loc f
            [
              (Nolabel, elist ~loc args);
              (Labelled "else_", stage_expr ~params else_);
            ]
      | Func ({ node = "coalesce"; _ } as name) ->
          let args, else_ =
            let rec loop args =
              match args with
              | [] ->
                  Location.raise_errorf ~loc "coalesce(..): empty argument list"
              | [ else_ ] -> ([], else_)
              | x :: args ->
                  let xs, else_ = loop args in
                  (x :: xs, else_)
            in
            loop args
          in
          let f =
            let loc = to_location name in
            map_operator_to_expr ~loc name.node (List.length args)
          in
          let args = List.map args ~f:(stage_expr ~params) in
          pexp_apply ~loc f
            [
              (Nolabel, elist ~loc args);
              (Labelled "else_", stage_expr ~params else_);
            ]
      | Func { node = "tuple"; _ } ->
          let arity = List.length args in
          let f =
            match arity with
            | 2 -> [%expr Ch_queries.Expr.tuple2]
            | 3 -> [%expr Ch_queries.Expr.tuple3]
            | 4 -> [%expr Ch_queries.Expr.tuple4]
            | _ ->
                Location.raise_errorf ~loc
                  "tuple(...) requires 2, 3, or 4 arguments"
          in
          let args = List.map args ~f:(stage_expr ~params) in
          eapply ~loc f [ pexp_tuple ~loc args ]
      | Func
          {
            node =
              ( "greatest" | "least" | "concat" | "midpoint" | "arrayConcat"
              | "arrayEnumerateUniq" | "arrayIntersect"
              | "arraySymmetricDifference" | "arrayUnion" | "arrayUniq"
              | "firstNonDefault" ) as name;
            _;
          } ->
          let f = evar ~loc ("Ch_queries.Expr." ^ name) in
          let args = List.map args ~f:(stage_expr ~params) in
          eapply ~loc f [ elist ~loc args ]
      | Func { node = "initializeAggregation"; _ } -> (
          match args with
          | agg_func :: rest ->
              let f = evar ~loc "Ch_queries.Expr.initializeAggregation" in
              let agg_func = stage_expr ~params agg_func in
              let rest = List.map rest ~f:(stage_expr ~params) in
              eapply ~loc f [ agg_func; elist ~loc rest ]
          | [] ->
              Location.raise_errorf ~loc
                "initializeAggregation requires an aggregate function name and \
                 at least one argument")
      | Func { node = "JSONExtract"; _ } ->
          let f = evar ~loc "Ch_queries.Expr.jSONExtract" in
          let args = List.map args ~f:(stage_expr ~params) in
          eapply ~loc f [ elist ~loc args ]
      | Func { node = "JSONExtractBool"; _ } ->
          let f = evar ~loc "Ch_queries.Expr.jSONExtractBool" in
          let args = List.map args ~f:(stage_expr ~params) in
          eapply ~loc f [ elist ~loc args ]
      | Func { node = "JSONExtractInt"; _ } ->
          let f = evar ~loc "Ch_queries.Expr.jSONExtractInt" in
          let args = List.map args ~f:(stage_expr ~params) in
          eapply ~loc f [ elist ~loc args ]
      | Func { node = "JSONExtractString"; _ } ->
          let f = evar ~loc "Ch_queries.Expr.jSONExtractString" in
          let args = List.map args ~f:(stage_expr ~params) in
          eapply ~loc f [ elist ~loc args ]
      | Func { node = "JSONExtractUInt"; _ } ->
          let f = evar ~loc "Ch_queries.Expr.jSONExtractUInt" in
          let args = List.map args ~f:(stage_expr ~params) in
          eapply ~loc f [ elist ~loc args ]
      | Func { node = "JSONHas"; _ } ->
          let f = evar ~loc "Ch_queries.Expr.jSONHas" in
          let args = List.map args ~f:(stage_expr ~params) in
          eapply ~loc f [ elist ~loc args ]
      | Func { node = "cityHash64"; _ } -> (
          match args with
          | [ arg ] ->
              let f = evar ~loc "Ch_queries.Expr.cityHash64" in
              let arg = stage_expr ~params arg in
              eapply ~loc f [ arg ]
          | _ ->
              let f = evar ~loc "Ch_queries.Expr.cityHash64_multi" in
              let args = List.map args ~f:(stage_expr ~params) in
              eapply ~loc f [ elist ~loc args ])
      | Func { node = "farmHash64"; _ } -> (
          match args with
          | [ arg ] ->
              let f = evar ~loc "Ch_queries.Expr.farmHash64" in
              let arg = stage_expr ~params arg in
              eapply ~loc f [ arg ]
          | _ ->
              let f = evar ~loc "Ch_queries.Expr.farmHash64_multi" in
              let args = List.map args ~f:(stage_expr ~params) in
              eapply ~loc f [ elist ~loc args ])
      | Func { node = "sipHash64"; _ } -> (
          match args with
          | [ arg ] ->
              let f = evar ~loc "Ch_queries.Expr.sipHash64" in
              let arg = stage_expr ~params arg in
              eapply ~loc f [ arg ]
          | _ ->
              let f = evar ~loc "Ch_queries.Expr.sipHash64_multi" in
              let args = List.map args ~f:(stage_expr ~params) in
              eapply ~loc f [ elist ~loc args ])
      | Func { node = "xxHash64"; _ } -> (
          match args with
          | [ arg ] ->
              let f = evar ~loc "Ch_queries.Expr.xxHash64" in
              let arg = stage_expr ~params arg in
              eapply ~loc f [ arg ]
          | _ ->
              let f = evar ~loc "Ch_queries.Expr.xxHash64_multi" in
              let args = List.map args ~f:(stage_expr ~params) in
              eapply ~loc f [ elist ~loc args ])
      | Func { node = "wordShingleMinHashCaseInsensitiveUTF8"; _ } -> (
          let f =
            evar ~loc "Ch_queries.Expr.wordShingleMinHashCaseInsensitiveUTF8"
          in
          match args with
          | [ str ] ->
              let str = stage_expr ~params str in
              eapply ~loc f [ str ]
          | [ str; shinglesize ] ->
              let str = stage_expr ~params str in
              let shinglesize = stage_expr ~params shinglesize in
              pexp_apply ~loc f
                [ (Labelled "shinglesize", shinglesize); (Nolabel, str) ]
          | [ str; shinglesize; hashnum ] ->
              let str = stage_expr ~params str in
              let shinglesize = stage_expr ~params shinglesize in
              let hashnum = stage_expr ~params hashnum in
              pexp_apply ~loc f
                [
                  (Labelled "shinglesize", shinglesize);
                  (Labelled "hashnum", hashnum);
                  (Nolabel, str);
                ]
          | _ ->
              Location.raise_errorf ~loc
                "wordShingleMinHashCaseInsensitiveUTF8 requires 1, 2, or 3 \
                 arguments")
      | Func { node = ("arrayZip" | "arrayZipUnaligned") as name; _ } -> (
          match args with
          | [ arr1; arr2 ] ->
              let f = evar ~loc ("Ch_queries.Expr." ^ name) in
              let arr1 = stage_expr ~params arr1 in
              let arr2 = stage_expr ~params arr2 in
              eapply ~loc f [ arr1; arr2 ]
          | _ ->
              Location.raise_errorf ~loc "%s requires exactly 2 array arguments"
                name)
      | Func { node = "arrayReduce"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.arrayReduce" in
          match args with
          | agg_func :: arrays when List.length arrays >= 1 ->
              let agg_func = stage_expr ~params agg_func in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ agg_func; elist ~loc arrays ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayReduce requires an aggregate function name and at least \
                 one array argument")
      | Func { node = "arrayReduceInRanges"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.arrayReduceInRanges" in
          match args with
          | agg_func :: ranges :: arrays when List.length arrays >= 1 ->
              let agg_func = stage_expr ~params agg_func in
              let ranges = stage_expr ~params ranges in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ agg_func; ranges; elist ~loc arrays ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayReduceInRanges requires an aggregate function name, \
                 ranges, and at least one array argument")
      | Func { node = "arrayAll"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.arrayAll" in
          match args with
          | lambda :: arrays when List.length arrays >= 1 ->
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayAll requires a lambda and at least one array argument")
      | Func { node = "arrayMap"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.arrayMap" in
          match args with
          | lambda :: arrays when List.length arrays >= 1 ->
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayMap requires a lambda and at least one array argument")
      | Func { node = "arrayExists"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.arrayExists" in
          match args with
          | lambda :: arrays when List.length arrays >= 1 ->
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayExists requires a lambda and at least one array argument")
      | Func { node = "arrayFill"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.arrayFill" in
          match args with
          | lambda :: arrays when List.length arrays >= 1 ->
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayFill requires a lambda and at least one array argument")
      | Func { node = "arrayReverseFill"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.arrayReverseFill" in
          match args with
          | lambda :: arrays when List.length arrays >= 1 ->
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayReverseFill requires a lambda and at least one array \
                 argument")
      | Func { node = "arrayReverseSplit"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.arrayReverseSplit" in
          match args with
          | lambda :: arrays when List.length arrays >= 1 ->
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayReverseSplit requires a lambda and at least one array \
                 argument")
      | Func { node = "arraySplit"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.arraySplit" in
          match args with
          | lambda :: arrays when List.length arrays >= 1 ->
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | _ ->
              Location.raise_errorf ~loc
                "arraySplit requires a lambda and at least one array argument")
      | Func { node = "arrayFirst"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.arrayFirst" in
          match args with
          | lambda :: arrays when List.length arrays >= 1 ->
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayFirst requires a lambda and at least one array argument")
      | Func { node = "arrayFirstIndex"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.arrayFirstIndex" in
          match args with
          | lambda :: arrays when List.length arrays >= 1 ->
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayFirstIndex requires a lambda and at least one array \
                 argument")
      | Func { node = "arrayFirstOrNull"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.arrayFirstOrNull" in
          match args with
          | lambda :: arrays when List.length arrays >= 1 ->
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayFirstOrNull requires a lambda and at least one array \
                 argument")
      | Func { node = "arrayLast"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.arrayLast" in
          match args with
          | lambda :: arrays when List.length arrays >= 1 ->
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayLast requires a lambda and at least one array argument")
      | Func { node = "arrayLastIndex"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.arrayLastIndex" in
          match args with
          | lambda :: arrays when List.length arrays >= 1 ->
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayLastIndex requires a lambda and at least one array \
                 argument")
      | Func { node = "arrayLastOrNull"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.arrayLastOrNull" in
          match args with
          | lambda :: arrays when List.length arrays >= 1 ->
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayLastOrNull requires a lambda and at least one array \
                 argument")
      | Func { node = "arrayFold"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.arrayFold" in
          match args with
          | lambda :: rest when List.length rest >= 2 ->
              (* arrayFold(lambda, arr1, ..., arrN, acc) *)
              let arrays, acc =
                let rev_rest = List.rev rest in
                match rev_rest with
                | acc :: rev_arrays -> (List.rev rev_arrays, acc)
                | _ -> assert false (* checked by length condition *)
              in
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              let acc = stage_expr ~params acc in
              eapply ~loc f [ lambda; elist ~loc arrays; acc ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayFold requires a lambda, at least one array, and an \
                 accumulator argument")
      | Func { node = "arrayAvg"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.arrayAvg" in
          match args with
          | lambda :: arrays when List.length arrays >= 1 ->
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayAvg requires a lambda and at least one array argument")
      | Func { node = "arrayCount"; _ } -> (
          match args with
          | ({ node = E_lambda _; _ } as lambda) :: arrays
            when List.length arrays >= 1 ->
              (* arrayCount(lambda, arr1, ...) - with lambda, use arrayCountF *)
              let f = evar ~loc "Ch_queries.Expr.arrayCountF" in
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | [ arr ] ->
              (* arrayCount(arr) - without lambda, use arrayCount *)
              let f = evar ~loc "Ch_queries.Expr.arrayCount" in
              let arr = stage_expr ~params arr in
              eapply ~loc f [ arr ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayCount requires exactly one array argument, or a lambda \
                 followed by one or more arrays")
      | Func { node = "arrayMax"; _ } -> (
          match args with
          | ({ node = E_lambda _; _ } as lambda) :: arrays
            when List.length arrays >= 1 ->
              (* arrayMax(lambda, arr1, ...) - with lambda, use arrayMaxF *)
              let f = evar ~loc "Ch_queries.Expr.arrayMaxF" in
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | [ arr ] ->
              (* arrayMax(arr) - without lambda, use arrayMax *)
              let f = evar ~loc "Ch_queries.Expr.arrayMax" in
              let arr = stage_expr ~params arr in
              eapply ~loc f [ arr ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayMax requires exactly one array argument, or a lambda \
                 followed by one or more arrays")
      | Func { node = "arrayMin"; _ } -> (
          match args with
          | ({ node = E_lambda _; _ } as lambda) :: arrays
            when List.length arrays >= 1 ->
              (* arrayMin(lambda, arr1, ...) - with lambda, use arrayMinF *)
              let f = evar ~loc "Ch_queries.Expr.arrayMinF" in
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | [ arr ] ->
              (* arrayMin(arr) - without lambda, use arrayMin *)
              let f = evar ~loc "Ch_queries.Expr.arrayMin" in
              let arr = stage_expr ~params arr in
              eapply ~loc f [ arr ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayMin requires exactly one array argument, or a lambda \
                 followed by one or more arrays")
      | Func { node = "arraySum"; _ } -> (
          match args with
          | ({ node = E_lambda _; _ } as lambda) :: arrays
            when List.length arrays >= 1 ->
              (* arraySum(lambda, arr1, ...) - with lambda, use arraySumF *)
              let f = evar ~loc "Ch_queries.Expr.arraySumF" in
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | [ arr ] ->
              (* arraySum(arr) - without lambda, use arraySum *)
              let f = evar ~loc "Ch_queries.Expr.arraySum" in
              let arr = stage_expr ~params arr in
              eapply ~loc f [ arr ]
          | _ ->
              Location.raise_errorf ~loc
                "arraySum requires exactly one array argument, or a lambda \
                 followed by one or more arrays")
      | Func { node = "arrayProduct"; _ } -> (
          match args with
          | ({ node = E_lambda _; _ } as lambda) :: arrays
            when List.length arrays >= 1 ->
              (* arrayProduct(lambda, arr1, ...) - with lambda, use arrayProductF *)
              let f = evar ~loc "Ch_queries.Expr.arrayProductF" in
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | [ arr ] ->
              (* arrayProduct(arr) - without lambda, use arrayProduct *)
              let f = evar ~loc "Ch_queries.Expr.arrayProduct" in
              let arr = stage_expr ~params arr in
              eapply ~loc f [ arr ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayProduct requires exactly one array argument, or a lambda \
                 followed by one or more arrays")
      | Func { node = "arrayCumSum"; _ } -> (
          match args with
          | ({ node = E_lambda _; _ } as lambda) :: arrays
            when List.length arrays >= 1 ->
              (* arrayCumSum(lambda, arr1, ...) - with lambda, use arrayCumSumF *)
              let f = evar ~loc "Ch_queries.Expr.arrayCumSumF" in
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | [ arr ] ->
              (* arrayCumSum(arr) - without lambda, use arrayCumSum *)
              let f = evar ~loc "Ch_queries.Expr.arrayCumSum" in
              let arr = stage_expr ~params arr in
              eapply ~loc f [ arr ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayCumSum requires exactly one array argument, or a lambda \
                 followed by one or more arrays")
      | Func { node = "arrayCumSumNonNegative"; _ } -> (
          match args with
          | ({ node = E_lambda _; _ } as lambda) :: arrays
            when List.length arrays >= 1 ->
              (* arrayCumSumNonNegative(lambda, arr1, ...) - with lambda, use arrayCumSumNonNegativeF *)
              let f = evar ~loc "Ch_queries.Expr.arrayCumSumNonNegativeF" in
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | [ arr ] ->
              (* arrayCumSumNonNegative(arr) - without lambda, use arrayCumSumNonNegative *)
              let f = evar ~loc "Ch_queries.Expr.arrayCumSumNonNegative" in
              let arr = stage_expr ~params arr in
              eapply ~loc f [ arr ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayCumSumNonNegative requires exactly one array argument, \
                 or a lambda followed by one or more arrays")
      | Func { node = "arrayReverseSort"; _ } -> (
          (* arrayReverseSort([f,] arr [, arr1, ... ,arrN]) *)
          match args with
          | ({ node = E_lambda _; _ } as lambda) :: rest
            when List.length rest >= 1 ->
              (* arrayReverseSort(lambda, arr1, ...) - with lambda, use arrayReverseSortF *)
              let f = evar ~loc "Ch_queries.Expr.arrayReverseSortF" in
              let lambda = stage_expr ~params lambda in
              let arrays = List.map rest ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | [ arr ] ->
              (* arrayReverseSort(arr) - without lambda, use arrayReverseSort *)
              let f = evar ~loc "Ch_queries.Expr.arrayReverseSort" in
              let arr = stage_expr ~params arr in
              eapply ~loc f [ arr ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayReverseSort requires exactly one array argument, or a \
                 lambda followed by one or more arrays")
      | Func { node = "arraySort"; _ } -> (
          (* arraySort([f,] arr [, arr1, ... ,arrN]) *)
          match args with
          | ({ node = E_lambda _; _ } as lambda) :: rest
            when List.length rest >= 1 ->
              (* arraySort(lambda, arr1, ...) - with lambda, use arraySortF *)
              let f = evar ~loc "Ch_queries.Expr.arraySortF" in
              let lambda = stage_expr ~params lambda in
              let arrays = List.map rest ~f:(stage_expr ~params) in
              eapply ~loc f [ lambda; elist ~loc arrays ]
          | [ arr ] ->
              (* arraySort(arr) - without lambda, use arraySort *)
              let f = evar ~loc "Ch_queries.Expr.arraySort" in
              let arr = stage_expr ~params arr in
              eapply ~loc f [ arr ]
          | _ ->
              Location.raise_errorf ~loc
                "arraySort requires exactly one array argument, or a lambda \
                 followed by one or more arrays")
      | Func { node = "arrayPartialReverseSort"; _ } -> (
          (* arrayPartialReverseSort([f,] arr [, arr1, ... ,arrN], limit) *)
          match args with
          | ({ node = E_lambda _; _ } as lambda) :: rest
            when List.length rest >= 2 ->
              (* arrayPartialReverseSort(lambda, arr1, ..., limit) - with lambda, use arrayPartialReverseSortF *)
              let f = evar ~loc "Ch_queries.Expr.arrayPartialReverseSortF" in
              let arrays, limit =
                let rev_rest = List.rev rest in
                match rev_rest with
                | limit :: rev_arrays -> (List.rev rev_arrays, limit)
                | _ -> assert false (* checked by length condition *)
              in
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              let limit = stage_expr ~params limit in
              eapply ~loc f [ lambda; elist ~loc arrays; limit ]
          | [ arr; limit ] ->
              (* arrayPartialReverseSort(arr, limit) - without lambda, use arrayPartialReverseSort *)
              let f = evar ~loc "Ch_queries.Expr.arrayPartialReverseSort" in
              let arr = stage_expr ~params arr in
              let limit = stage_expr ~params limit in
              eapply ~loc f [ arr; limit ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayPartialReverseSort requires exactly one array and a \
                 limit argument, or a lambda followed by one or more arrays \
                 and a limit")
      | Func { node = "arrayPartialSort"; _ } -> (
          (* arrayPartialSort([f,] arr [, arr1, ... ,arrN], limit) *)
          match args with
          | ({ node = E_lambda _; _ } as lambda) :: rest
            when List.length rest >= 2 ->
              (* arrayPartialSort(lambda, arr1, ..., limit) - with lambda, use arrayPartialSortF *)
              let f = evar ~loc "Ch_queries.Expr.arrayPartialSortF" in
              let arrays, limit =
                let rev_rest = List.rev rest in
                match rev_rest with
                | limit :: rev_arrays -> (List.rev rev_arrays, limit)
                | _ -> assert false (* checked by length condition *)
              in
              let lambda = stage_expr ~params lambda in
              let arrays = List.map arrays ~f:(stage_expr ~params) in
              let limit = stage_expr ~params limit in
              eapply ~loc f [ lambda; elist ~loc arrays; limit ]
          | [ arr; limit ] ->
              (* arrayPartialSort(arr, limit) - without lambda, use arrayPartialSort *)
              let f = evar ~loc "Ch_queries.Expr.arrayPartialSort" in
              let arr = stage_expr ~params arr in
              let limit = stage_expr ~params limit in
              eapply ~loc f [ arr; limit ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayPartialSort requires exactly one array and a limit \
                 argument, or a lambda followed by one or more arrays and a \
                 limit")
      | Func { node = "divideDecimal"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.divideDecimal" in
          match args with
          | [ x; y ] ->
              let x = stage_expr ~params x in
              let y = stage_expr ~params y in
              eapply ~loc f [ x; y ]
          | [ x; y; result_scale ] ->
              let x = stage_expr ~params x in
              let y = stage_expr ~params y in
              let result_scale = stage_expr ~params result_scale in
              pexp_apply ~loc f
                [
                  (Labelled "result_scale", result_scale);
                  (Nolabel, x);
                  (Nolabel, y);
                ]
          | _ ->
              Location.raise_errorf ~loc
                "divideDecimal requires 2 or 3 arguments")
      | Func { node = "multiplyDecimal"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.multiplyDecimal" in
          match args with
          | [ x; y ] ->
              let x = stage_expr ~params x in
              let y = stage_expr ~params y in
              eapply ~loc f [ x; y ]
          | [ x; y; result_scale ] ->
              let x = stage_expr ~params x in
              let y = stage_expr ~params y in
              let result_scale = stage_expr ~params result_scale in
              pexp_apply ~loc f
                [
                  (Labelled "result_scale", result_scale);
                  (Nolabel, x);
                  (Nolabel, y);
                ]
          | _ ->
              Location.raise_errorf ~loc
                "multiplyDecimal requires 2 or 3 arguments")
      | Func { node = "arrayAUCPR"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.arrayAUCPR" in
          match args with
          | [ scores; labels ] ->
              let scores = stage_expr ~params scores in
              let labels = stage_expr ~params labels in
              eapply ~loc f [ scores; labels ]
          | [ scores; labels; partial_offsets ] ->
              let scores = stage_expr ~params scores in
              let labels = stage_expr ~params labels in
              let partial_offsets = stage_expr ~params partial_offsets in
              pexp_apply ~loc f
                [
                  (Labelled "partial_offsets", partial_offsets);
                  (Nolabel, scores);
                  (Nolabel, labels);
                ]
          | _ ->
              Location.raise_errorf ~loc "arrayAUCPR requires 2 or 3 arguments")
      | Func { node = "arrayROCAUC"; _ } -> (
          let f = evar ~loc "Ch_queries.Expr.arrayROCAUC" in
          match args with
          | [ scores; labels ] ->
              let scores = stage_expr ~params scores in
              let labels = stage_expr ~params labels in
              eapply ~loc f [ scores; labels ]
          | [ scores; labels; scale ] ->
              let scores = stage_expr ~params scores in
              let labels = stage_expr ~params labels in
              let scale = stage_expr ~params scale in
              pexp_apply ~loc f
                [
                  (Labelled "scale", scale); (Nolabel, scores); (Nolabel, labels);
                ]
          | [ scores; labels; scale; partial_offsets ] ->
              let scores = stage_expr ~params scores in
              let labels = stage_expr ~params labels in
              let scale = stage_expr ~params scale in
              let partial_offsets = stage_expr ~params partial_offsets in
              pexp_apply ~loc f
                [
                  (Labelled "scale", scale);
                  (Labelled "partial_offsets", partial_offsets);
                  (Nolabel, scores);
                  (Nolabel, labels);
                ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayROCAUC requires 2, 3, or 4 arguments")
      | Func { node = "arrayPartialShuffle"; _ } -> (
          (* arrayPartialShuffle(arr [, limit[, seed]]) *)
          let f = evar ~loc "Ch_queries.Expr.arrayPartialShuffle" in
          match args with
          | [ arr ] ->
              let arr = stage_expr ~params arr in
              eapply ~loc f [ arr ]
          | [ arr; limit ] ->
              let arr = stage_expr ~params arr in
              let limit = stage_expr ~params limit in
              pexp_apply ~loc f [ (Labelled "limit", limit); (Nolabel, arr) ]
          | [ arr; limit; seed ] ->
              let arr = stage_expr ~params arr in
              let limit = stage_expr ~params limit in
              let seed = stage_expr ~params seed in
              pexp_apply ~loc f
                [
                  (Labelled "limit", limit);
                  (Labelled "seed", seed);
                  (Nolabel, arr);
                ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayPartialShuffle requires 1, 2, or 3 arguments")
      | Func { node = "arrayShuffle"; _ } -> (
          (* arrayShuffle(arr [, seed]) *)
          let f = evar ~loc "Ch_queries.Expr.arrayShuffle" in
          match args with
          | [ arr ] ->
              let arr = stage_expr ~params arr in
              eapply ~loc f [ arr ]
          | [ arr; seed ] ->
              let arr = stage_expr ~params arr in
              let seed = stage_expr ~params seed in
              pexp_apply ~loc f [ (Labelled "seed", seed); (Nolabel, arr) ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayShuffle requires 1 or 2 arguments")
      | Func { node = "arrayResize"; _ } -> (
          (* arrayResize(arr, size[, extender]) *)
          let f = evar ~loc "Ch_queries.Expr.arrayResize" in
          match args with
          | [ arr; size ] ->
              let arr = stage_expr ~params arr in
              let size = stage_expr ~params size in
              eapply ~loc f [ arr; size ]
          | [ arr; size; extender ] ->
              let arr = stage_expr ~params arr in
              let size = stage_expr ~params size in
              let extender = stage_expr ~params extender in
              pexp_apply ~loc f
                [
                  (Labelled "extender", extender);
                  (Nolabel, arr);
                  (Nolabel, size);
                ]
          | _ ->
              Location.raise_errorf ~loc "arrayResize requires 2 or 3 arguments"
          )
      | Func { node = "arrayShiftLeft"; _ } -> (
          (* arrayShiftLeft(arr, n[, default]) *)
          let f = evar ~loc "Ch_queries.Expr.arrayShiftLeft" in
          match args with
          | [ arr; n ] ->
              let arr = stage_expr ~params arr in
              let n = stage_expr ~params n in
              eapply ~loc f [ arr; n ]
          | [ arr; n; default ] ->
              let arr = stage_expr ~params arr in
              let n = stage_expr ~params n in
              let default = stage_expr ~params default in
              pexp_apply ~loc f
                [ (Labelled "default", default); (Nolabel, arr); (Nolabel, n) ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayShiftLeft requires 2 or 3 arguments")
      | Func { node = "arrayShiftRight"; _ } -> (
          (* arrayShiftRight(arr, n[, default]) *)
          let f = evar ~loc "Ch_queries.Expr.arrayShiftRight" in
          match args with
          | [ arr; n ] ->
              let arr = stage_expr ~params arr in
              let n = stage_expr ~params n in
              eapply ~loc f [ arr; n ]
          | [ arr; n; default ] ->
              let arr = stage_expr ~params arr in
              let n = stage_expr ~params n in
              let default = stage_expr ~params default in
              pexp_apply ~loc f
                [ (Labelled "default", default); (Nolabel, arr); (Nolabel, n) ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayShiftRight requires 2 or 3 arguments")
      | Func { node = "arraySlice"; _ } -> (
          (* arraySlice(arr, offset[, length]) *)
          let f = evar ~loc "Ch_queries.Expr.arraySlice" in
          match args with
          | [ arr; offset ] ->
              let arr = stage_expr ~params arr in
              let offset = stage_expr ~params offset in
              eapply ~loc f [ arr; offset ]
          | [ arr; offset; length ] ->
              let arr = stage_expr ~params arr in
              let offset = stage_expr ~params offset in
              let length = stage_expr ~params length in
              pexp_apply ~loc f
                [
                  (Labelled "length", length); (Nolabel, arr); (Nolabel, offset);
                ]
          | _ ->
              Location.raise_errorf ~loc "arraySlice requires 2 or 3 arguments")
      | Func { node = "range"; _ } -> (
          (* range(end) or range(start, end) or range(start, end, step) *)
          let f = evar ~loc "Ch_queries.Expr.range" in
          match args with
          | [ end_ ] ->
              let end_ = stage_expr ~params end_ in
              eapply ~loc f [ end_ ]
          | [ start; end_ ] ->
              let start = stage_expr ~params start in
              let end_ = stage_expr ~params end_ in
              pexp_apply ~loc f [ (Labelled "start", start); (Nolabel, end_) ]
          | [ start; end_; step ] ->
              let start = stage_expr ~params start in
              let end_ = stage_expr ~params end_ in
              let step = stage_expr ~params step in
              pexp_apply ~loc f
                [
                  (Labelled "start", start);
                  (Labelled "step", step);
                  (Nolabel, end_);
                ]
          | _ ->
              Location.raise_errorf ~loc "range requires 1, 2, or 3 arguments")
      | Func { node = "trunc"; _ } -> (
          (* trunc(x[, n]) *)
          let f = evar ~loc "Ch_queries.Expr.trunc" in
          match args with
          | [ x ] ->
              let x = stage_expr ~params x in
              eapply ~loc f [ x ]
          | [ x; n ] ->
              let x = stage_expr ~params x in
              let n = stage_expr ~params n in
              pexp_apply ~loc f [ (Labelled "n", n); (Nolabel, x) ]
          | _ -> Location.raise_errorf ~loc "trunc requires 1 or 2 arguments")
      | Func { node = "arrayStringConcat"; _ } -> (
          (* arrayStringConcat(arr[, separator]) *)
          let f = evar ~loc "Ch_queries.Expr.arrayStringConcat" in
          match args with
          | [ arr ] ->
              let arr = stage_expr ~params arr in
              eapply ~loc f [ arr ]
          | [ arr; separator ] ->
              let arr = stage_expr ~params arr in
              let separator = stage_expr ~params separator in
              pexp_apply ~loc f
                [ (Labelled "separator", separator); (Nolabel, arr) ]
          | _ ->
              Location.raise_errorf ~loc
                "arrayStringConcat requires 1 or 2 arguments")
      | Func { node = "splitByChar"; _ } -> (
          (* splitByChar(sep, str[, max_substrings]) *)
          let f = evar ~loc "Ch_queries.Expr.splitByChar" in
          match args with
          | [ sep; str ] ->
              let sep = stage_expr ~params sep in
              let str = stage_expr ~params str in
              eapply ~loc f [ sep; str ]
          | [ sep; str; max_substrings ] ->
              let sep = stage_expr ~params sep in
              let str = stage_expr ~params str in
              let max_substrings = stage_expr ~params max_substrings in
              pexp_apply ~loc f
                [
                  (Labelled "max_substrings", max_substrings);
                  (Nolabel, sep);
                  (Nolabel, str);
                ]
          | _ ->
              Location.raise_errorf ~loc "splitByChar requires 2 or 3 arguments"
          )
      | Func { node = "match"; _ } ->
          (* match is a reserved keyword in OCaml, so we use match_ *)
          let f = evar ~loc "Ch_queries.Expr.match_" in
          let args = List.map args ~f:(stage_expr ~params) in
          eapply ~loc f args
      | Func { node = "length"; _ } -> (
          (* length(x::String) or length('literal') -> stringLength, otherwise length for arrays *)
          match args with
          | [
           { node = E_ascribe (e, { node = T { node = "String"; _ }; _ }); _ };
          ] ->
              let f = evar ~loc "Ch_queries.Expr.stringLength" in
              let e = stage_expr ~params e in
              eapply ~loc f [ e ]
          | [ ({ node = E_lit (L_string _); _ } as e) ] ->
              let f = evar ~loc "Ch_queries.Expr.stringLength" in
              let e = stage_expr ~params e in
              eapply ~loc f [ e ]
          | _ ->
              let f = evar ~loc "Ch_queries.Expr.length" in
              let args = List.map args ~f:(stage_expr ~params) in
              eapply ~loc f args)
      | Func { node = "trimBoth"; _ } -> (
          (* trimBoth(str[, trim_characters]) *)
          let f = evar ~loc "Ch_queries.Expr.trimBoth" in
          match args with
          | [ str ] ->
              let str = stage_expr ~params str in
              eapply ~loc f [ str ]
          | [ str; trim_characters ] ->
              let str = stage_expr ~params str in
              let trim_characters = stage_expr ~params trim_characters in
              pexp_apply ~loc f
                [
                  (Labelled "trim_characters", trim_characters); (Nolabel, str);
                ]
          | _ -> Location.raise_errorf ~loc "trimBoth requires 1 or 2 arguments"
          )
      | Func { node = "trimRight"; _ } -> (
          (* trimRight(str[, trim_characters]) *)
          let f = evar ~loc "Ch_queries.Expr.trimRight" in
          match args with
          | [ str ] ->
              let str = stage_expr ~params str in
              eapply ~loc f [ str ]
          | [ str; trim_characters ] ->
              let str = stage_expr ~params str in
              let trim_characters = stage_expr ~params trim_characters in
              pexp_apply ~loc f
                [
                  (Labelled "trim_characters", trim_characters); (Nolabel, str);
                ]
          | _ ->
              Location.raise_errorf ~loc "trimRight requires 1 or 2 arguments")
      | Func { node = "substring"; _ } -> (
          (* substring(str, offset[, length]) *)
          let f = evar ~loc "Ch_queries.Expr.substring" in
          match args with
          | [ str; offset ] ->
              let str = stage_expr ~params str in
              let offset = stage_expr ~params offset in
              eapply ~loc f [ str; offset ]
          | [ str; offset; length ] ->
              let str = stage_expr ~params str in
              let offset = stage_expr ~params offset in
              let length = stage_expr ~params length in
              pexp_apply ~loc f
                [
                  (Labelled "length", length); (Nolabel, str); (Nolabel, offset);
                ]
          | _ ->
              Location.raise_errorf ~loc "substring requires 2 or 3 arguments")
      | Func { node = "substringUTF8"; _ } -> (
          (* substringUTF8(str, offset[, length]) *)
          let f = evar ~loc "Ch_queries.Expr.substringUTF8" in
          match args with
          | [ str; offset ] ->
              let str = stage_expr ~params str in
              let offset = stage_expr ~params offset in
              eapply ~loc f [ str; offset ]
          | [ str; offset; length ] ->
              let str = stage_expr ~params str in
              let offset = stage_expr ~params offset in
              let length = stage_expr ~params length in
              pexp_apply ~loc f
                [
                  (Labelled "length", length); (Nolabel, str); (Nolabel, offset);
                ]
          | _ ->
              Location.raise_errorf ~loc
                "substringUTF8 requires 2 or 3 arguments")
      | Func { node = ("joinGet" | "joinGetOrNull") as fname; _ } ->
          (* joinGet(DICT, VALUE, KEYS...) stages as:
             Ch_queries.Expr.joinGet
               (DICT : _ Ch_queries.Dict.t)
               DICT.Ch_queries.Dict.values#VALUE
               tuple<N>(KEYS...) *)
          let dict, value_arg, key_args =
            match args with
            | dict :: value :: keys -> (dict, value, keys)
            | _ ->
                Location.raise_errorf ~loc
                  "%s requires at least 2 arguments (dict, value)" fname
          in
          (* Stage dict - db.table, 'db.table', or $param *)
          let dict =
            match dict.Syntax.node with
            | E_col (db, table) -> evar ~loc (refer_to_db_table db table)
            | E_lit (L_string s) -> (
                match String.split_on_char ~by:'.' s with
                | [ db; table ] ->
                    let db_id = Syntax.make_id db in
                    let table_id = Syntax.make_id table in
                    evar ~loc (refer_to_db_table db_id table_id)
                | _ ->
                    Location.raise_errorf ~loc
                      "%s: dict string must be 'db.table'" fname)
            | E_param { param; param_has_scope; param_optional } ->
                if param_has_scope then
                  Location.raise_errorf ~loc
                    "%s: $.%s: scope-accessing parameters are not allowed for \
                     dict"
                    fname param.Syntax.node;
                if param_optional then
                  Location.raise_errorf ~loc
                    "%s: ?%s: optional parameters are not allowed for dict"
                    fname param.Syntax.node;

                evar ~loc:(to_location param) param.Syntax.node
            | _ ->
                Location.raise_errorf ~loc
                  "%s: dict must be db.table, 'db.table', or $param" fname
          in
          let dict = [%expr ([%e dict] : _ Ch_queries.Dict.t)] in
          (* Stage value - either 'literal' -> dict.values#literal or $param *)
          let value_expr =
            match value_arg.Syntax.node with
            | E_lit (L_string value_name) ->
                let scope = [%expr [%e dict].Ch_queries.Dict.values] in
                let q = pexp_send ~loc scope (Located.mk ~loc "query") in
                [%expr
                  [%e q] (fun __q ->
                      [%e
                        pexp_send ~loc [%expr __q] (Located.mk ~loc value_name)])]
            | E_param { param; param_has_scope; param_optional } ->
                if param_optional then
                  Location.raise_errorf ~loc
                    "%s: ?%s: optional parameters are not allowed for dict \
                     value"
                    fname param.Syntax.node;
                let e = evar ~loc:(to_location param) param.Syntax.node in
                let scope = [%expr [%e dict].Ch_queries.Dict.values] in
                let q = pexp_send ~loc scope (Located.mk ~loc "query") in
                if param_has_scope then [%expr [%e q] (fun __q -> [%e e] __q)]
                else [%expr [%e q] (fun _ -> [%e e])]
            | _ ->
                Location.raise_errorf ~loc
                  "%s: value must be 'literal' or $param" fname
          in
          (* Stage keys - 1 to 4 keys, wrapped in tuple if multiple *)
          let num_keys = List.length key_args in
          if num_keys = 0 then
            Location.raise_errorf ~loc "%s requires at least 1 key argument"
              fname
          else if num_keys > 4 then
            Location.raise_errorf ~loc "%s supports at most 4 key arguments"
              fname
          else
            let key_exprs = List.map key_args ~f:(stage_expr ~params) in
            let keys_expr =
              match key_exprs with
              | [ k ] -> k
              | [ k1; k2 ] -> [%expr Ch_queries.Expr.tuple2 ([%e k1], [%e k2])]
              | [ k1; k2; k3 ] ->
                  [%expr Ch_queries.Expr.tuple3 ([%e k1], [%e k2], [%e k3])]
              | [ k1; k2; k3; k4 ] ->
                  [%expr
                    Ch_queries.Expr.tuple4 ([%e k1], [%e k2], [%e k3], [%e k4])]
              | _ -> assert false
            in
            let f = evar ~loc (Printf.sprintf "Ch_queries.Expr.%s" fname) in
            eapply ~loc f [ dict; value_expr; keys_expr ]
      | Func { node = "dictGet"; _ } ->
          (* dictGet(DICT, VALUE, KEY) stages as:
             Ch_queries.Expr.dictGet
               (DICT : _ Ch_queries.Dict.t)
               DICT.Ch_queries.Dict.values#VALUE
               KEY *)
          let dict, value_arg, key_arg =
            match args with
            | [ dict; value; key ] -> (dict, value, key)
            | _ ->
                Location.raise_errorf ~loc
                  "dictGet requires exactly 3 arguments (dict, value, key)"
          in
          (* Stage dict - db.table, 'db.table', or $param *)
          let dict =
            match dict.Syntax.node with
            | E_col (db, table) -> evar ~loc (refer_to_db_table db table)
            | E_lit (L_string s) -> (
                match String.split_on_char ~by:'.' s with
                | [ db; table ] ->
                    let db_id = Syntax.make_id db in
                    let table_id = Syntax.make_id table in
                    evar ~loc (refer_to_db_table db_id table_id)
                | _ ->
                    Location.raise_errorf ~loc
                      "dictGet: dict string must be 'db.table'")
            | E_param { param; param_has_scope; param_optional } ->
                if param_has_scope then
                  Location.raise_errorf ~loc
                    "dictGet: $.%s: scope-accessing parameters are not allowed \
                     for dict"
                    param.Syntax.node;
                if param_optional then
                  Location.raise_errorf ~loc
                    "dictGet: ?%s: optional parameters are not allowed for dict"
                    param.Syntax.node;

                evar ~loc:(to_location param) param.Syntax.node
            | _ ->
                Location.raise_errorf ~loc
                  "dictGet: dict must be db.table, 'db.table', or $param"
          in
          let dict = [%expr ([%e dict] : _ Ch_queries.Dict.t)] in
          (* Stage value - either 'literal' -> dict.values#literal or $param *)
          let value_expr =
            match value_arg.Syntax.node with
            | E_lit (L_string value_name) ->
                let scope = [%expr [%e dict].Ch_queries.Dict.values] in
                let q = pexp_send ~loc scope (Located.mk ~loc "query") in
                [%expr
                  [%e q] (fun __q ->
                      [%e
                        pexp_send ~loc [%expr __q] (Located.mk ~loc value_name)])]
            | E_param { param; param_has_scope; param_optional } ->
                if param_optional then
                  Location.raise_errorf ~loc
                    "dictGet: ?%s: optional parameters are not allowed for \
                     dict value"
                    param.Syntax.node;
                let e = evar ~loc:(to_location param) param.Syntax.node in
                let scope = [%expr [%e dict].Ch_queries.Dict.values] in
                let q = pexp_send ~loc scope (Located.mk ~loc "query") in
                if param_has_scope then [%expr [%e q] (fun __q -> [%e e] __q)]
                else [%expr [%e q] (fun _ -> [%e e])]
            | _ ->
                Location.raise_errorf ~loc
                  "dictGet: value must be 'literal' or $param"
          in
          (* Stage key - single expression *)
          let key_expr = stage_expr ~params key_arg in
          eapply ~loc [%expr Ch_queries.Expr.dictGet]
            [ dict; value_expr; key_expr ]
      | Func { node = "age"; _ } -> (
          (* age(unit, startdate, enddate[, timezone]) *)
          let f = evar ~loc "Ch_queries.Expr.age" in
          match args with
          | [ unit; startdate; enddate ] ->
              let unit = stage_expr ~params unit in
              let startdate = stage_expr ~params startdate in
              let enddate = stage_expr ~params enddate in
              eapply ~loc f [ unit; startdate; enddate ]
          | [ unit; startdate; enddate; timezone ] ->
              let unit = stage_expr ~params unit in
              let startdate = stage_expr ~params startdate in
              let enddate = stage_expr ~params enddate in
              let timezone = stage_expr ~params timezone in
              pexp_apply ~loc f
                [
                  (Labelled "timezone", timezone);
                  (Nolabel, unit);
                  (Nolabel, startdate);
                  (Nolabel, enddate);
                ]
          | _ -> Location.raise_errorf ~loc "age requires 3 or 4 arguments")
      | Func { node = "dateDiff"; _ } -> (
          (* dateDiff(unit, startdate, enddate[, timezone]) *)
          let f = evar ~loc "Ch_queries.Expr.dateDiff" in
          match args with
          | [ unit; startdate; enddate ] ->
              let unit = stage_expr ~params unit in
              let startdate = stage_expr ~params startdate in
              let enddate = stage_expr ~params enddate in
              eapply ~loc f [ unit; startdate; enddate ]
          | [ unit; startdate; enddate; timezone ] ->
              let unit = stage_expr ~params unit in
              let startdate = stage_expr ~params startdate in
              let enddate = stage_expr ~params enddate in
              let timezone = stage_expr ~params timezone in
              pexp_apply ~loc f
                [
                  (Labelled "timezone", timezone);
                  (Nolabel, unit);
                  (Nolabel, startdate);
                  (Nolabel, enddate);
                ]
          | _ -> Location.raise_errorf ~loc "dateDiff requires 3 or 4 arguments"
          )
      | Func { node = "formatDateTime"; _ } -> (
          (* formatDateTime(datetime, format[, timezone]) *)
          let f = evar ~loc "Ch_queries.Expr.formatDateTime" in
          match args with
          | [ datetime; format ] ->
              let datetime = stage_expr ~params datetime in
              let format = stage_expr ~params format in
              eapply ~loc f [ datetime; format ]
          | [ datetime; format; timezone ] ->
              let datetime = stage_expr ~params datetime in
              let format = stage_expr ~params format in
              let timezone = stage_expr ~params timezone in
              pexp_apply ~loc f
                [
                  (Labelled "timezone", timezone);
                  (Nolabel, datetime);
                  (Nolabel, format);
                ]
          | _ ->
              Location.raise_errorf ~loc
                "formatDateTime requires 2 or 3 arguments")
      | Func { node = "timeSlots"; _ } -> (
          (* timeSlots(start_time, duration[, size]) *)
          let f = evar ~loc "Ch_queries.Expr.timeSlots" in
          match args with
          | [ start_time; duration ] ->
              let start_time = stage_expr ~params start_time in
              let duration = stage_expr ~params duration in
              eapply ~loc f [ start_time; duration ]
          | [ start_time; duration; size ] ->
              let start_time = stage_expr ~params start_time in
              let duration = stage_expr ~params duration in
              let size = stage_expr ~params size in
              pexp_apply ~loc f
                [
                  (Labelled "size", size);
                  (Nolabel, start_time);
                  (Nolabel, duration);
                ]
          | _ ->
              Location.raise_errorf ~loc "timeSlots requires 2 or 3 arguments")
      | Func { node = "toYearWeek"; _ } -> (
          (* toYearWeek(date[, mode[, timezone]]) *)
          let f = evar ~loc "Ch_queries.Expr.toYearWeek" in
          match args with
          | [ date ] ->
              let date = stage_expr ~params date in
              eapply ~loc f [ date ]
          | [ date; mode ] ->
              let date = stage_expr ~params date in
              let mode = stage_expr ~params mode in
              pexp_apply ~loc f [ (Labelled "mode", mode); (Nolabel, date) ]
          | [ date; mode; timezone ] ->
              let date = stage_expr ~params date in
              let mode = stage_expr ~params mode in
              let timezone = stage_expr ~params timezone in
              pexp_apply ~loc f
                [
                  (Labelled "mode", mode);
                  (Labelled "timezone", timezone);
                  (Nolabel, date);
                ]
          | _ ->
              Location.raise_errorf ~loc
                "toYearWeek requires 1, 2, or 3 arguments")
      | Func name ->
          let f =
            let loc = to_location name in
            map_operator_to_expr ~loc name.node (List.length args)
          in
          let args =
            match args with
            | [] -> [ [%expr ()] ]
            | args -> List.map args ~f:(stage_expr ~params)
          in
          eapply ~loc f args
      | Func_method (scope, method_name) ->
          refer_to_scope ~loc scope method_name ~map:(fun e ->
              let staged_args = List.map args ~f:(stage_expr ~params) in
              eapply ~loc e staged_args))
  | Syntax.E_param { param; param_has_scope; param_optional } -> (
      if param_optional then
        Location.raise_errorf ~loc
          "?%s: optional parameters are not allowed in expressions" param.node;
      let e = evar ~loc param.node in
      match param_has_scope with true -> [%expr [%e e] __q] | false -> e)
  | Syntax.E_ocaml_expr ocaml_code -> parse_ocaml_expr ~loc ocaml_code
  | Syntax.E_in (expr, in_query) -> (
      let expr = stage_expr ~params expr in
      match in_query with
      | Syntax.In_query query ->
          let query = stage_query query in
          [%expr Ch_queries.in_ [%e expr] (Ch_queries.In_query [%e query])]
      | Syntax.In_expr
          { node = E_param { param; param_has_scope; param_optional }; _ } ->
          (* special for [E in $param], we don't treat it as expression *)
          let loc = to_location param in
          if param_has_scope then
            Location.raise_errorf ~loc
              "$.%s: scope-accessing parameters are not allowed in IN clause"
              param.node;
          if param_optional then
            Location.raise_errorf ~loc
              "?%s: optional parameters are not allowed in IN clause" param.node;

          let param = evar ~loc param.node in
          [%expr Ch_queries.in_ [%e expr] [%e param]]
      | Syntax.In_expr
          {
            node =
              E_ascribe
                ( {
                    node = E_param { param; param_has_scope; param_optional };
                    loc;
                    _;
                  },
                  t );
            _;
          }
        when Syntax.is_scope_typ t ->
          (* special for [E in $param::T], we treat this according to T (is it a scope or not?) *)
          (if param_has_scope then
             let loc = to_location param in
             Location.raise_errorf ~loc
               "$.%s: scope-accessing parameters are not allowed in this \
                context"
               param.node);
          (if param_optional then
             let loc = to_location param in
             Location.raise_errorf ~loc
               "?%s: optional parameters are not allowed in this context"
               param.node);
          let query =
            Syntax.make_query ~loc
              (Q_param
                 { param; param_has_scope = false; param_optional = false })
          in
          let loc = to_location param in
          let query = stage_query query in
          [%expr Ch_queries.in_ [%e expr] (Ch_queries.In_query [%e query])]
      | Syntax.In_expr expr' ->
          let expr' = stage_expr ~params expr' in
          [%expr Ch_queries.in_ [%e expr] (Ch_queries.In_array [%e expr'])])
  | Syntax.E_lambda (lambda_params, body) -> (
      let all_params = List.rev_append lambda_params params in
      let body_expr = stage_expr ~params:all_params body in
      match lambda_params with
      | [ param ] ->
          let param_name = param.node in
          [%expr
            Ch_queries.lambda [%e estring ~loc param_name] (fun _ ->
                [%e body_expr])]
      | [ param1; param2 ] ->
          let param1_name = param1.node in
          let param2_name = param2.node in
          [%expr
            Ch_queries.lambda2 [%e estring ~loc param1_name]
              [%e estring ~loc param2_name] (fun x y -> [%e body_expr])]
      | _ ->
          Location.raise_errorf ~loc
            "lambdas with more than 2 parameters are not supported")

and stage_dimensions ~loc dimensions =
  let body =
    List.map dimensions ~f:(function
      | Syntax.Dimension_splice p ->
          let e = Syntax.make_expr ~loc:p.param.loc (E_param p) in
          stage_expr ~params:[] e
      | Syntax.Dimension_expr expr ->
          let expr = stage_expr ~params:[] expr in
          [%expr [ Ch_queries.A_expr [%e expr] ]])
  in
  [%expr List.concat [%e elist ~loc body]]

and stage_fill ~loc fill =
  let fill_from =
    match fill.Syntax.fill_from with
    | None -> [%expr None]
    | Some e ->
        let e = stage_expr ~params:[] e in
        [%expr Some (Ch_queries.A_expr [%e e])]
  in
  let fill_to =
    match fill.fill_to with
    | None -> [%expr None]
    | Some e ->
        let e = stage_expr ~params:[] e in
        [%expr Some (Ch_queries.A_expr [%e e])]
  in
  let fill_step =
    match fill.fill_step with
    | None -> [%expr None]
    | Some e ->
        let e = stage_expr ~params:[] e in
        [%expr Some (Ch_queries.A_expr [%e e])]
  in
  let fill_interpolate =
    let items_expr =
      List.map fill.fill_interpolate
        ~f:(fun { Syntax.interpolate_col; interpolate_expr } ->
          let col_str = estring ~loc interpolate_col.Syntax.node in
          let expr_opt =
            match interpolate_expr with
            | None -> [%expr None]
            | Some e ->
                let e = stage_expr ~params:[] e in
                [%expr Some (Ch_queries.A_expr [%e e])]
          in
          [%expr [%e col_str], [%e expr_opt]])
    in
    elist ~loc items_expr
  in
  [%expr
    {
      Ch_queries.fill_from = [%e fill_from];
      fill_to = [%e fill_to];
      fill_step = [%e fill_step];
      fill_interpolate = [%e fill_interpolate];
    }]

(** Generate order_by for window functions - uses simple (a_expr * dir) tuples
*)
and stage_window_order_by ~loc order_by =
  let xs =
    List.map order_by ~f:(function
      | Syntax.Order_by_splice p ->
          let e = Syntax.make_expr ~loc:p.param.loc (E_param p) in
          stage_expr ~params:[] e
      | Syntax.Order_by_expr (expr, dir, _fill) ->
          (* fill is ignored for window functions *)
          let expr = stage_expr ~params:[] expr in
          let dir =
            match dir with `ASC -> [%expr `ASC] | `DESC -> [%expr `DESC]
          in
          [%expr [ (Ch_queries.A_expr [%e expr], [%e dir]) ]])
  in
  [%expr List.concat [%e elist ~loc xs]]

and stage_order_by ~loc order_by =
  let xs =
    List.map order_by ~f:(function
      | Syntax.Order_by_splice p ->
          let e = Syntax.make_expr ~loc:p.param.loc (E_param p) in
          stage_expr ~params:[] e
      | Syntax.Order_by_expr (expr, dir, fill) ->
          let expr = stage_expr ~params:[] expr in
          let dir =
            match dir with `ASC -> [%expr `ASC] | `DESC -> [%expr `DESC]
          in
          let fill =
            match fill with
            | None -> [%expr None]
            | Some fill -> [%expr Some [%e stage_fill ~loc fill]]
          in
          [%expr [ (Ch_queries.A_expr [%e expr], [%e dir], [%e fill]) ]])
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
            | Syntax.Setting_param { param; param_has_scope; param_optional } ->
                if param_has_scope then
                  Location.raise_errorf ~loc
                    "scope-accessing parameters are not allowed in settings";
                if param_optional then
                  Location.raise_errorf ~loc
                    "?%s: optional parameters are not allowed in settings"
                    param.node;
                let e =
                  Syntax.make_expr ~loc:param.loc
                    (E_param
                       {
                         param;
                         param_has_scope = false;
                         param_optional = false;
                       })
                in
                stage_expr ~params:[] e
          in
          [%expr [ ([%e id_str], [%e value_expr]) ]]
      | Syntax.Setting_splice p ->
          let e = Syntax.make_expr ~loc:p.param.loc (E_param p) in
          stage_expr ~params:[] e)
  in
  [%expr List.concat [%e elist ~loc body]]

and stage_field idx { Syntax.expr; alias } =
  let idx = idx + 1 in
  let loc = to_location expr in
  let name =
    match alias with
    | Some name -> located_of_id name
    | None ->
        let name =
          match extract_alias expr with
          | Some id -> id
          | None -> Syntax.make_id ~loc:expr.loc ("_" ^ string_of_int idx)
        in
        located_of_id name
  in
  let expr = stage_expr ~params:[] expr in
  (pcf_method ~loc (name, Public, Cfk_concrete (Fresh, expr)), alias)

and stage_query ({ node; _ } as q) =
  match node with
  | Q_ascribe (q, t) ->
      let loc = to_location q in
      let q = stage_query q in
      let t = stage_typ t in
      [%expr ([%e q] : [%t t] Ch_queries.select)]
  | Q_union (q1, q2) ->
      let loc = to_location q in
      let q1 = stage_query q1 in
      let q2 = stage_query q2 in
      pexp_apply ~loc [%expr Ch_queries.union] [ (Nolabel, q1); (Nolabel, q2) ]
  | Q_param { param; param_has_scope; param_optional } ->
      let loc = to_location param in
      if param_has_scope then
        Location.raise_errorf ~loc
          "$.%s: scope-accessing parameters are not allowed for query params"
          param.node;
      if param_optional then
        Location.raise_errorf ~loc
          "?%s: optional parameters are not allowed for query params" param.node;

      evar ~loc param.node
  | Q_select
      {
        with_fields;
        select;
        from;
        prewhere = _;
        where = _;
        qualify = _;
        group_by = _;
        having = _;
        order_by = _;
        limit = _;
        offset = _;
        settings = _;
      } ->
      let loc = to_location q in
      let scope_of_with, ctes =
        List.partition_filter_map with_fields ~f:(function
          | Syntax.With_expr ({ alias = Some id; _ } as field) ->
              `Left (id, fst (stage_field 0 field))
          | Syntax.With_expr { alias = None; expr } ->
              let loc = to_location expr in
              Location.raise_errorf ~loc "WITH <expr> requires an alias"
          | Syntax.With_query (alias, query, materialized) ->
              let loc = to_location query in
              let materialized = ebool ~loc materialized in
              let query = stage_query query in
              `Right (loc, alias, query, materialized))
      in
      let select, scopes =
        match select with
        | Syntax.Select_fields fields ->
            let fields = List.mapi fields ~f:stage_field in
            let pcstr_fields =
              List.map fields ~f:(function
                | expr, None -> expr
                | _expr, Some alias ->
                    let name = located_of_id alias in
                    let expr = pexp_send ~loc [%expr __q] name in
                    pcf_method ~loc (name, Public, Cfk_concrete (Fresh, expr)))
            in
            let select_obj =
              pexp_object ~loc { pcstr_self = ppat_any ~loc; pcstr_fields }
            in
            let scope_of_select =
              List.filter_map fields ~f:(function
                | m, Some alias -> Some (alias, m)
                | _, None -> None)
            in
            (make_hole ~loc select_obj, [ scope_of_with; scope_of_select ])
        | Syntax.Select_splice { param; param_has_scope; param_optional } ->
            let loc = to_location param in
            if param_optional then
              Location.raise_errorf ~loc
                "?%s: optional parameters are not allowed in SELECT splice"
                param.node;
            let scope =
              match param_has_scope with
              | true -> [%expr fun __q -> [%e evar ~loc param.node] __q]
              | false -> [%expr fun __q -> [%e evar ~loc param.node]]
            in
            (scope, [ scope_of_with ])
      in
      let args =
        let from =
          [%expr
            Ch_queries.map_from_scope [%e stage_from from]
              (fun [%p from_scope_pattern from] ->
                [%e from_scope_expr ~scopes from])]
        in
        [ (Labelled "select", select); (Labelled "from", from) ]
      in
      let args = stage_query_args args q in
      let query =
        pexp_apply ~loc [%expr Ch_queries.select]
          ((Nolabel, [%expr ()]) :: List.rev args)
      in
      let query =
        List.fold_left (List.rev ctes) ~init:query
          ~f:(fun query (loc, alias, query', materialized) ->
            [%expr
              Ch_queries.with_cte ~materialized:[%e materialized]
                ~alias:[%e estring ~loc alias.Syntax.node] [%e query']
                (fun [%p pvar ~loc alias.Syntax.node] -> [%e query])])
      in
      query

and stage_query_args args ({ Ch_queries_syntax.Syntax.node; _ } as q) =
  match node with
  | Ch_queries_syntax.Syntax.Q_select
      {
        with_fields = _;
        select = _;
        from = _;
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
      let args =
        match prewhere with
        | None -> args
        | Some prewhere ->
            let loc = to_location prewhere in
            let prewhere = make_hole ~loc (stage_expr ~params:[] prewhere) in
            (Labelled "prewhere", prewhere) :: args
      in
      let args =
        match where with
        | None -> args
        | Some where -> stage_clause_expr ~label:"where" where :: args
      in
      let args =
        match qualify with
        | None -> args
        | Some qualify -> stage_clause_expr ~label:"qualify" qualify :: args
      in
      let args =
        match group_by with
        | None -> args
        | Some dimensions ->
            let loc = to_location q in
            let group_by = stage_dimensions ~loc dimensions in
            let group_by = make_hole ~loc group_by in
            (Labelled "group_by", group_by) :: args
      in
      let args =
        match having with
        | None -> args
        | Some having -> stage_clause_expr ~label:"having" having :: args
      in
      let args =
        match order_by with
        | None -> args
        | Some
            [
              Order_by_splice
                { param; param_has_scope; param_optional = true };
            ] ->
            if not param_has_scope then
              Location.raise_errorf ~loc:(to_location param)
                "?$%s: optional parameters must use scope access syntax \
                 (?$.%s)"
                param.Syntax.node param.Syntax.node;
            let var = evar ~loc:(to_location param) param.Syntax.node in
            (Optional "order_by", var) :: args
        | Some order_by_items ->
            let loc = to_location q in
            let order_by = stage_order_by ~loc order_by_items in
            let order_by = make_hole ~loc order_by in
            (Labelled "order_by", order_by) :: args
      in
      let args =
        match limit with
        | None -> args
        | Some limit -> stage_clause_expr ~label:"limit" limit :: args
      in
      let args =
        match offset with
        | None -> args
        | Some offset -> stage_clause_expr ~label:"offset" offset :: args
      in
      let args =
        match settings with
        | [] -> args
        | settings ->
            let loc = to_location q in
            let settings_expr = stage_settings ~loc settings in
            (Labelled "settings", settings_expr) :: args
      in
      args
  | Q_union _ | Q_ascribe _ | Q_param _ -> failwith "invariant violation"

and stage_field_syntax { Syntax.expr; alias } =
  let loc = to_location expr in
  let alias =
    match alias with
    | Some name -> name.node
    | None -> (
        match extract_alias expr with
        | Some id -> id.node
        | None ->
            Location.raise_errorf ~loc
              "%%ch.query_syntax: field requires an explicit alias")
  in
  let expr = stage_expr ~params:[] expr in
  (alias, [%expr Ch_queries.A_field ([%e expr], [%e estring ~loc alias])])

and stage_query_syntax ({ Syntax.node; _ } as q) =
  match node with
  | Syntax.Q_select
      {
        with_fields;
        select;
        from;
        prewhere = _;
        where = _;
        qualify = _;
        group_by = _;
        having = _;
        order_by = _;
        limit = _;
        offset = _;
        settings = _;
      } ->
      let loc = to_location q in
      (match with_fields with
      | [] -> ()
      | _ ->
          Location.raise_errorf ~loc
            "WITH is not supported in %%ch.query_syntax");
      let select =
        match select with
        | Syntax.Select_fields fields ->
            let seen = Hashtbl.create 16 in
            let fields =
              List.map fields ~f:(fun field ->
                  let alias, expr = stage_field_syntax field in
                  if Hashtbl.mem seen alias then
                    Location.raise_errorf ~loc
                      "%%ch.query_syntax: duplicate field alias '%s'" alias
                  else Hashtbl.add seen alias ();
                  expr)
            in
            make_hole ~loc (elist ~loc fields)
        | Syntax.Select_splice _ ->
            Location.raise_errorf ~loc
              "%%ch.query_syntax: splice is not supported"
      in
      let args =
        let from =
          [%expr
            Ch_queries.map_from_scope [%e stage_from from]
              (fun [%p from_scope_pattern from] ->
                [%e from_scope_expr ~scopes:[] from])]
        in
        [ (Labelled "select", select); (Labelled "from", from) ]
      in
      let args = stage_query_args args q in
      pexp_apply ~loc [%expr Ch_queries.select_syntax]
        ((Nolabel, [%expr ()]) :: List.rev args)
  | _ ->
      let loc = to_location q in
      Location.raise_errorf ~loc
        "only SELECT query is supported in %%ch.query_syntax"

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
            let __q = [%e from_scope_expr ~scopes:[] from] in
            [%e stage_expr ~params:[] on]]
      in
      [%expr [%e f] [%e stage_from base] [%e stage_from_one join] ~on:[%e on]]

and stage_from_one from_one =
  let open Syntax in
  let loc = to_location from_one in
  match from_one.node with
  | F_table { db; table; alias; final } ->
      let qname = refer_to_db_table db table in
      [%expr
        [%e evar ~loc qname] ~alias:[%e estring ~loc alias.node]
          ~final:[%e ebool ~loc final]]
  | F_select { select; alias; cluster_name } ->
      let select = stage_query select in
      let alias = estring ~loc alias.node in
      let args = [ (Nolabel, select); (Labelled "alias", alias) ] in
      let args =
        match cluster_name with
        | None -> args
        | Some (Cluster_name id) ->
            let loc = to_location id in
            (Labelled "cluster_name", estring ~loc id.node) :: args
        | Some (Cluster_name_param { param; param_has_scope; param_optional })
          ->
            let loc = to_location param in
            if param_has_scope then
              Location.raise_errorf ~loc
                "$.%s: scope-accessing parameters are not allowed for cluster \
                 name"
                param.node;
            if param_optional then
              Location.raise_errorf ~loc
                "?%s: optional parameters are not allowed for cluster name"
                param.node;

            (Labelled "cluster_name", evar ~loc param.node) :: args
      in
      pexp_apply ~loc [%expr Ch_queries.from_select] args
  | F_param { param = { param; param_has_scope; param_optional }; alias; final }
    -> (
      let loc = to_location param in
      if param_has_scope then
        Location.raise_errorf ~loc
          "$.%s: scope-accessing parameters are not allowed for FROM clause"
          param.node;
      if param_optional then
        Location.raise_errorf ~loc
          "?%s: optional parameters are not allowed for FROM clause" param.node;

      match final with
      | false ->
          [%expr
            ([%e evar ~loc param.node] : alias:string -> _ Ch_queries.from_one)
              ~alias:[%e estring ~loc alias.node]]
      | true ->
          [%expr
            ([%e evar ~loc param.node]
              : final:bool -> alias:string -> _ Ch_queries.from_one)
              ~final:true ~alias:[%e estring ~loc alias.node]])
  | F_ascribe (from_one, t) ->
      let from_one = stage_from_one from_one in
      let t = stage_typ t in
      [%expr ([%e from_one] : [%t t] Ch_queries.from_one)]

let expand_query ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let query = parse_query ~loc txt in
      stage_query query
  | _ -> Location.raise_errorf "expected a string literal for the '%%q"

let expand_query_syntax ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let query = parse_query ~loc txt in
      stage_query_syntax query
  | _ -> Location.raise_errorf "expected a string literal for %%ch.query_syntax"

let expand_from ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let from = parse_from ~loc txt in
      [%expr
        Ch_queries.map_from_scope [%e stage_from from]
          (fun [%p from_scope_pattern from] ->
            [%e from_scope_expr ~scopes:[] from])]
  | _ -> Location.raise_errorf "expected a string literal for the '%%f"

let expand_expr ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let expr = parse_expr ~loc txt in
      stage_expr ~params:[] expr
  | _ -> Location.raise_errorf "expected a string literal for the '%%e"

let expand_uexpr ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let expr = parse_uexpr ~loc txt in
      stage_expr ~params:[] expr
  | _ -> Location.raise_errorf "expected a string literal for the '%%eu"

let expand_typ ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let typ = parse_typ ~loc txt in
      stage_typ typ
  | _ -> Location.raise_errorf "expected a string literal for [%%t ...]"

let expand_parser ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let typ = parse_typ ~loc txt in
      stage_typ_to_parser typ
  | _ -> Location.raise_errorf "expected a string literal for [%%ch.parser ...]"

let expand_scope ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let cols, is_open = parse_scope_columns ~loc txt in
      let object_type = stage_scope_columns ~loc ~is_open cols in
      [%pat? (__q : [%t object_type])]
  | _ -> Location.raise_errorf "expected a string literal for [%%scope ...]"

let expand_scope_type ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let cols, is_open = parse_scope_columns ~loc txt in
      stage_scope_columns ~loc ~is_open cols
  | _ -> Location.raise_errorf "expected a string literal for [%%scope ...]"

let rec extract_typed_fields query =
  match query.Syntax.node with
  | Syntax.Q_select { select = Select_fields fields; _ } ->
      List.mapi fields ~f:(fun idx { Syntax.expr; alias } ->
          let typ =
            match expr.node with
            | E_ascribe (_, typ) -> typ
            | _ -> Syntax.make_typ T_any
          in
          let name =
            match alias with
            | Some alias -> id_to_located alias
            | None -> (
                match extract_alias expr with
                | Some id -> id_to_located id
                | None ->
                    {
                      txt = Printf.sprintf "_%d" (idx + 1);
                      loc = to_location expr;
                    })
          in
          (name, typ))
  | Q_union (x, _) -> extract_typed_fields x
  | Q_select { select = Select_splice _; _ } ->
      Location.raise_errorf ~loc:(to_location query)
        "cannot extract fields from a query with dynamic scope"
  | Q_param _ ->
      Location.raise_errorf ~loc:(to_location query)
        "cannot extract fields from a query parameter"
  | Q_ascribe (q, _) -> extract_typed_fields q

let expand_query_and_map ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (query_str, loc, _)) ->
      let query = parse_query ~loc query_str in
      let fields = extract_typed_fields query in
      let query_expr = stage_query query in
      (* Build the Row expressions with let bindings to ensure evaluation order *)
      let bindings =
        List.map fields ~f:(fun (name, typ) ->
            let parser = stage_typ_to_parser typ in
            let col =
              stage_expr ~params:[]
                Syntax.(make_expr (E_col (make_id "q", make_id name.txt)))
            in
            ( { txt = Printf.sprintf "__col_%s" name.txt; loc = name.loc },
              [%expr Ch_queries.Row.col [%e col] [%e parser]] ))
      in
      let parser =
        match List.rev bindings with
        | [] ->
            Location.raise_errorf ~loc
              "%%ch.query_and_map requires at least one typed field"
        | (init, _) :: rest ->
            List.fold_left rest ~init:(evar ~loc:init.loc init.txt)
              ~f:(fun acc (v, _) ->
                [%expr
                  Ch_queries.Row.( and+ ) [%e evar ~loc:v.loc v.txt] [%e acc]])
      in
      let parser =
        List.fold_right bindings ~init:parser ~f:(fun (v, expr) acc ->
            [%expr
              let [%p pvar ~loc:v.loc v.txt] = [%e expr] in
              [%e acc]])
      in
      (* Pattern for destructuring the Row result: (field1, (field2, field3)) *)
      let destruct =
        let make (name, _) = pvar ~loc:name.loc name.txt in
        match List.rev fields with
        | [] -> failwith "impossible: checked above"
        | init :: rest ->
            List.fold_left rest ~init:(make init) ~f:(fun acc field ->
                [%pat? [%p make field], [%p acc]])
      in
      (* Application of f with labeled arguments *)
      let apply_f =
        let args =
          List.map fields ~f:(fun (name, _) ->
              (Labelled name.txt, evar ~loc:name.loc name.txt))
        in
        pexp_apply ~loc [%expr __f] args
      in
      [%expr
        let __query = [%e query_expr] in
        let __sql, __base_parse =
          Ch_queries.query __query (fun (__q : < q : _ Ch_queries.scope >) ->
              Ch_queries.Row.( let+ ) [%e parser] Fun.id)
        in
        let __map __row __f =
          let [%p destruct] = __base_parse __row in
          [%e apply_f]
        in
        (__sql, __map)]
  | _ ->
      Location.raise_errorf "expected a string literal for %%ch.query_and_map"

let query_extension name =
  Extension.V3.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_query

let query_syntax_extension name =
  Extension.V3.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_query_syntax

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

let parser_extension name =
  Extension.V3.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_parser

let uexpr_extension name =
  Extension.V3.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_uexpr

let scope_extension name =
  Extension.V3.declare name Extension.Context.pattern
    Ast_pattern.(single_expr_payload __)
    expand_scope

let scope_type_extension name =
  Extension.V3.declare name Extension.Context.core_type
    Ast_pattern.(single_expr_payload __)
    expand_scope_type

let query_and_map_extension name =
  Extension.V3.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_query_and_map

let rules =
  [
    Context_free.Rule.extension (query_extension "ch.q");
    Context_free.Rule.extension (query_extension "ch.query");
    Context_free.Rule.extension (query_syntax_extension "ch.query_syntax");
    Context_free.Rule.extension (from_extension "ch.f");
    Context_free.Rule.extension (from_extension "ch.from");
    Context_free.Rule.extension (expr_extension "ch.e");
    Context_free.Rule.extension (expr_extension "ch.expr");
    Context_free.Rule.extension (typ_extension "ch.t");
    Context_free.Rule.extension (typ_extension "ch.type");
    Context_free.Rule.extension (parser_extension "ch.parser");
    Context_free.Rule.extension (uexpr_extension "ch.eu");
    Context_free.Rule.extension (uexpr_extension "ch.expr_unsafe");
    Context_free.Rule.extension (query_and_map_extension "ch.query_and_map");
    Context_free.Rule.extension (scope_extension "ch.scope");
    Context_free.Rule.extension (scope_extension "ch.s");
    Context_free.Rule.extension (scope_type_extension "ch.scope");
    Context_free.Rule.extension (scope_type_extension "ch.s");
  ]

let () = Driver.register_transformation ~rules "queries_ppx"
