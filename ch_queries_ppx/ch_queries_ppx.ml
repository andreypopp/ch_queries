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

type from_ctx = From_none | From

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

let rec stage_typ' typ =
  let open Syntax in
  let loc = to_location typ in
  match typ.node with
  | T { node = "Date"; _ } ->
      (`NON_NULL, [%type: Ch_queries.date Ch_queries.timestamp])
  | T { node = "DateTime64"; _ } ->
      (`NON_NULL, [%type: Ch_queries.datetime64 Ch_queries.timestamp])
  | T { node = "DateTime"; _ } ->
      (`NON_NULL, [%type: Ch_queries.datetime Ch_queries.timestamp])
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
      (nullable, t)
  | T_db_table (db, table, nullable) ->
      let lid = Longident.parse (refer_to_db_table db table) in
      let lid = Located.mk ~loc lid in
      let t = ptyp_constr ~loc lid [] in
      (nullable, t)

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
  else [%type: ([%t nullable n], [%t t]) Ch_queries.expr]

let rec stage_typ_to_parser typ =
  let open Syntax in
  let loc = to_location typ in
  match typ.node with
  | T { node = "Date"; _ } -> [%expr Ch_queries.Row.date]
  | T { node = "DateTime64"; _ } ->
      Location.raise_errorf ~loc "parsing DateTime64 is not supported"
  | T { node = "DateTime"; _ } -> [%expr Ch_queries.Row.datetime]
  | T { node = "String"; _ } -> [%expr Ch_queries.Row.string]
  | T { node = "Bool"; _ } -> [%expr Ch_queries.Row.bool]
  | T { node = "Int8" | "UInt8" | "Int16" | "UInt16" | "Int32" | "UInt32"; _ }
    ->
      [%expr Ch_queries.Row.int]
  | T { node = "Int64"; _ } -> [%expr Ch_queries.Row.int64]
  | T { node = "UInt64"; _ } -> [%expr Ch_queries.Row.uint64]
  | T { node = "Float32" | "Float64" | "Float"; _ } ->
      [%expr Ch_queries.Row.float]
  | T { node = t; _ } ->
      Location.raise_errorf ~loc "unknown ClickHouse type: %s" t
  | T_app ({ node = "Nullable"; _ }, [ t ]) ->
      [%expr Ch_queries.Row.nullable [%e stage_typ_to_parser t]]
  | T_app ({ node = "Nullable"; _ }, _) ->
      Location.raise_errorf ~loc "Nullable(..) requires exactly one argument"
  | T_app ({ node = "Array"; _ }, [ t ]) ->
      [%expr Ch_queries.Row.array [%e stage_typ_to_parser t]]
  | T_app ({ node = "Array"; _ }, _) ->
      Location.raise_errorf ~loc "Array(..) requires exactly one argument"
  | T_app ({ node = "Map"; _ }, [ k; v ]) ->
      [%expr
        Ch_queries.Row.map [%e stage_typ_to_parser k] [%e stage_typ_to_parser v]]
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

let rec stage_typ_to_ocaml_type typ =
  let open Syntax in
  let loc = to_location typ in
  match typ.node with
  | T { node = "Date"; _ } -> [%type: float]
  | T { node = "DateTime64"; _ } ->
      Location.raise_errorf ~loc "parsing DateTime64 is not supported"
  | T { node = "DateTime"; _ } -> [%type: float]
  | T { node = "String"; _ } -> [%type: string]
  | T { node = "Bool"; _ } -> [%type: bool]
  | T { node = "Int8" | "UInt8" | "Int16" | "UInt16" | "Int32" | "UInt32"; _ }
    ->
      [%type: int]
  | T { node = "Int64"; _ } -> [%type: int64]
  | T { node = "UInt64"; _ } -> [%type: Unsigned.uint64]
  | T { node = "Float32" | "Float64" | "Float"; _ } -> [%type: float]
  | T { node = t; _ } ->
      Location.raise_errorf ~loc "unknown ClickHouse type: %s" t
  | T_app ({ node = "Nullable"; _ }, [ t ]) ->
      [%type: [%t stage_typ_to_ocaml_type t] option]
  | T_app ({ node = "Nullable"; _ }, _) ->
      Location.raise_errorf ~loc "Nullable(..) requires exactly one argument"
  | T_app ({ node = "Array"; _ }, [ t ]) ->
      [%type: [%t stage_typ_to_ocaml_type t] list]
  | T_app ({ node = "Array"; _ }, _) ->
      Location.raise_errorf ~loc "Array(..) requires exactly one argument"
  | T_app ({ node = "Map"; _ }, [ k; v ]) ->
      [%type:
        ([%t stage_typ_to_ocaml_type k] * [%t stage_typ_to_ocaml_type v]) list]
  | T_app ({ node = "Map"; _ }, _) ->
      Location.raise_errorf ~loc "Map(..) requires exactly two argument"
  | T_app ({ node = "Tuple"; _ }, _) ->
      Location.raise_errorf ~loc "parsing Tuple(..) is not supported"
  | T_app ({ node = t; _ }, _) ->
      Location.raise_errorf ~loc "Unknown ClickHouse type: %s" t
  | T_scope _ ->
      Location.raise_errorf ~loc
        "OCaml type conversion for scope types is not supported"
  | T_db_table _ ->
      Location.raise_errorf ~loc
        "OCaml type conversion for table types is not supported"

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
  | name, _ -> evar ~loc ("Ch_queries.Expr." ^ name)

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

let rec stage_expr ~on_evar ~params ~(from : from_ctx) expr =
  let loc = to_location expr in
  match expr.node with
  | Syntax.E_ascribe (e, t) ->
      let e = stage_expr ~on_evar ~params ~from e in
      [%expr ([%e e] : [%t stage_typ t])]
  | Syntax.E_unsafe_concat xs ->
      let xs =
        List.map xs ~f:(fun e ->
            [%expr Ch_queries.A_expr [%e stage_expr ~on_evar ~params ~from e]])
      in
      [%expr Ch_queries.unsafe_concat [%e elist ~loc xs]]
  | Syntax.E_unsafe id ->
      let loc = to_location id in
      [%expr Ch_queries.unsafe [%e estring ~loc id.node]]
  | Syntax.E_col (scope, id) -> refer_to_scope ~loc scope id
  | Syntax.E_query (scope, expr) ->
      query_scope ~loc scope (stage_expr ~on_evar ~params:[] ~from:From expr)
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
        List.map args ~f:(fun arg ->
            (Nolabel, stage_expr ~on_evar ~params ~from arg))
      in
      let args =
        match partition_by with
        | None -> args
        | Some dimensions ->
            ( Labelled "partition_by",
              stage_dimensions ~on_evar ~loc ~from dimensions )
            :: args
      in
      let args =
        match order_by with
        | None -> args
        | Some order_by ->
            (Labelled "order_by", stage_order_by ~on_evar ~loc ~from order_by)
            :: args
      in
      pexp_apply ~loc f args
  | Syntax.E_call (func, args) -> (
      match func with
      | Func { node = "["; _ } ->
          let xs =
            elist ~loc (List.map args ~f:(stage_expr ~on_evar ~params ~from))
          in
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
                      [
                        stage_expr ~on_evar ~params ~from k;
                        stage_expr ~on_evar ~params ~from v;
                      ])
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
            | args -> List.map args ~f:(stage_expr ~on_evar ~params ~from)
          in
          eapply ~loc f args
      | Func_method (scope, method_name) ->
          refer_to_scope ~loc scope method_name ~map:(fun e ->
              let staged_args =
                List.map args ~f:(stage_expr ~on_evar ~params ~from)
              in
              eapply ~loc e staged_args))
  | Syntax.E_param var -> (
      on_evar var;
      let e = evar ~loc var.node in
      match from with From_none -> e | From -> [%expr [%e e] __q])
  | Syntax.E_ocaml_expr ocaml_code -> parse_ocaml_expr ~loc ocaml_code
  | Syntax.E_in (expr, in_query) -> (
      let expr = stage_expr ~on_evar ~params ~from expr in
      match in_query with
      | Syntax.In_query query ->
          let query = stage_query ~on_evar query in
          [%expr Ch_queries.in_ [%e expr] (Ch_queries.In_query [%e query])]
      | Syntax.In_expr { node = E_param param; _ } ->
          (* special for [E in $param], we don't treat it as expression *)
          let loc = to_location param in
          on_evar param;
          let param = param.node in
          let param = evar ~loc param in
          [%expr Ch_queries.in_ [%e expr] [%e param]]
      | Syntax.In_expr
          { node = E_ascribe ({ node = E_param param; loc; _ }, t); _ }
        when Syntax.is_scope_typ t ->
          (* special for [E in $param::T], we treat this according to T (is it a scope or not?) *)
          let query = Syntax.make_query ~loc (Q_param param) in
          let loc = to_location param in
          let query = stage_query ~on_evar query in
          [%expr Ch_queries.in_ [%e expr] (Ch_queries.In_query [%e query])]
      | Syntax.In_expr expr' ->
          let expr' = stage_expr ~on_evar ~params ~from expr' in
          [%expr Ch_queries.in_ [%e expr] (Ch_queries.In_array [%e expr'])])
  | Syntax.E_lambda (param, body) ->
      let param_name = param.node in
      let body_expr =
        stage_expr ~on_evar ~params:(param :: params) ~from body
      in
      [%expr
        Ch_queries.lambda [%e estring ~loc param_name] (fun x -> [%e body_expr])]

and stage_dimensions ~on_evar ~loc ~from dimensions =
  let body =
    List.map dimensions ~f:(function
      | Syntax.Dimension_splice id ->
          let e = Syntax.make_expr ~loc:id.loc (E_param id) in
          stage_expr ~on_evar ~params:[] ~from e
      | Syntax.Dimension_expr expr ->
          let expr = stage_expr ~on_evar ~params:[] ~from expr in
          [%expr [ Ch_queries.A_expr [%e expr] ]])
  in
  [%expr List.concat [%e elist ~loc body]]

and stage_order_by ~on_evar ~loc ~from order_by =
  let xs =
    List.map order_by ~f:(function
      | Syntax.Order_by_splice id ->
          let e = Syntax.make_expr ~loc:id.loc (E_param id) in
          stage_expr ~on_evar ~params:[] ~from e
      | Syntax.Order_by_expr (expr, dir) ->
          let expr = stage_expr ~on_evar ~params:[] ~from expr in
          let dir =
            match dir with `ASC -> [%expr `ASC] | `DESC -> [%expr `DESC]
          in
          [%expr [ (Ch_queries.A_expr [%e expr], [%e dir]) ]])
  in
  [%expr List.concat [%e elist ~loc xs]]

and stage_settings ~on_evar ~loc settings =
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
                stage_expr ~on_evar ~params:[] ~from:From_none e
          in
          [%expr [ ([%e id_str], [%e value_expr]) ]]
      | Syntax.Setting_splice id ->
          let e = Syntax.make_expr ~loc:id.loc (E_param id) in
          stage_expr ~on_evar ~params:[] ~from:From_none e)
  in
  [%expr List.concat [%e elist ~loc body]]

and stage_field ~on_evar ~from idx { Syntax.expr; alias } =
  let idx = idx + 1 in
  let loc = to_location expr in
  let name =
    match alias with
    | Some name -> located_of_id name
    | None ->
        let name =
          match expr.node with
          | E_id id -> id
          | E_col (_, id) -> id
          | E_param id -> id
          | _ -> Syntax.make_id ~loc:expr.loc ("_" ^ string_of_int idx)
        in
        located_of_id name
  in
  let expr = stage_expr ~on_evar ~params:[] ~from expr in
  (pcf_method ~loc (name, Public, Cfk_concrete (Fresh, expr)), alias)

and stage_query ~on_evar ({ node; _ } as q) =
  match node with
  | Q_ascribe (q, t) ->
      let loc = to_location q in
      let q = stage_query ~on_evar q in
      let t = stage_typ t in
      [%expr ([%e q] : [%t t] Ch_queries.select)]
  | Q_union (q1, q2) ->
      let loc = to_location q in
      let q1 = stage_query ~on_evar q1 in
      let q2 = stage_query ~on_evar q2 in
      pexp_apply ~loc [%expr Ch_queries.union] [ (Nolabel, q1); (Nolabel, q2) ]
  | Q_param id ->
      on_evar id;
      let loc = to_location id in
      evar ~loc id.node
  | Q_select
      {
        with_fields;
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
      let scope_of_with, ctes =
        List.partition_filter_map with_fields ~f:(function
          | Syntax.With_expr ({ alias = Some id; _ } as field) ->
              `Left (id, fst (stage_field ~on_evar ~from:From 0 field))
          | Syntax.With_expr { alias = None; expr } ->
              let loc = to_location expr in
              Location.raise_errorf ~loc "WITH <expr> requires an alias"
          | Syntax.With_query (alias, query, materialized) ->
              let loc = to_location query in
              let materialized = ebool ~loc materialized in
              let query = stage_query ~on_evar query in
              `Right (loc, alias, query, materialized))
      in
      let select, scopes =
        match select with
        | Syntax.Select_fields fields ->
            let fields =
              List.mapi fields ~f:(stage_field ~on_evar ~from:From)
            in
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
        | Syntax.Select_splice id ->
            on_evar id;
            let loc = to_location id in
            (evar ~loc id.node, [ scope_of_with ])
      in
      let args =
        let from =
          [%expr
            Ch_queries.map_from_scope [%e stage_from ~on_evar from]
              (fun [%p from_scope_pattern from] ->
                [%e from_scope_expr ~scopes from])]
        in
        [ (Labelled "select", select); (Labelled "from", from) ]
      in
      let args =
        match prewhere with
        | None -> args
        | Some prewhere ->
            let loc = to_location prewhere in
            let prewhere =
              make_hole ~loc
                (stage_expr ~on_evar ~params:[] ~from:From prewhere)
            in
            (Labelled "prewhere", prewhere) :: args
      in
      let args =
        match where with
        | None -> args
        | Some where ->
            let loc = to_location where in
            let where =
              make_hole ~loc (stage_expr ~on_evar ~params:[] ~from:From where)
            in
            (Labelled "where", where) :: args
      in
      let args =
        match qualify with
        | None -> args
        | Some qualify ->
            let loc = to_location qualify in
            let qualify =
              make_hole ~loc (stage_expr ~on_evar ~params:[] ~from:From qualify)
            in
            (Labelled "qualify", qualify) :: args
      in
      let args =
        match group_by with
        | None -> args
        | Some dimensions ->
            let loc = to_location q in
            let group_by =
              stage_dimensions ~on_evar ~loc ~from:From dimensions
            in
            let group_by = make_hole ~loc group_by in
            (Labelled "group_by", group_by) :: args
      in
      let args =
        match having with
        | None -> args
        | Some having ->
            let loc = to_location having in
            let having =
              make_hole ~loc (stage_expr ~on_evar ~params:[] ~from:From having)
            in
            (Labelled "having", having) :: args
      in
      let args =
        match order_by with
        | None -> args
        | Some order_by ->
            let loc = to_location q in
            let order_by = stage_order_by ~on_evar ~loc ~from:From order_by in
            let order_by = make_hole ~loc order_by in
            (Labelled "order_by", order_by) :: args
      in
      let args =
        match limit with
        | None -> args
        | Some expr ->
            let limit =
              make_hole ~loc (stage_expr ~on_evar ~params:[] ~from:From expr)
            in
            (Labelled "limit", limit) :: args
      in
      let args =
        match offset with
        | None -> args
        | Some expr ->
            let offset =
              make_hole ~loc (stage_expr ~on_evar ~params:[] ~from:From expr)
            in
            (Labelled "offset", offset) :: args
      in
      let args =
        match settings with
        | [] -> args
        | settings ->
            let loc = to_location q in
            let settings_expr = stage_settings ~on_evar ~loc settings in
            (Labelled "settings", settings_expr) :: args
      in
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

and stage_from ~on_evar from =
  let open Syntax in
  let loc = to_location from in
  match from.node with
  | F from_one -> [%expr Ch_queries.from [%e stage_from_one ~on_evar from_one]]
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
            [%e stage_expr ~on_evar ~params:[] ~from:From on]]
      in
      [%expr
        [%e f] [%e stage_from ~on_evar base]
          [%e stage_from_one ~on_evar join]
          ~on:[%e on]]

and stage_from_one ~on_evar from_one =
  let open Syntax in
  let loc = to_location from_one in
  match from_one.node with
  | F_table { db; table; alias; final } ->
      let qname = refer_to_db_table db table in
      [%expr
        [%e evar ~loc qname] ~alias:[%e estring ~loc alias.node]
          ~final:[%e ebool ~loc final]]
  | F_select { select; alias; cluster_name } ->
      let select = stage_query ~on_evar select in
      let alias = estring ~loc alias.node in
      let args = [ (Nolabel, select); (Labelled "alias", alias) ] in
      let args =
        match cluster_name with
        | None -> args
        | Some (Cluster_name id) ->
            let loc = to_location id in
            (Labelled "cluster_name", estring ~loc id.node) :: args
        | Some (Cluster_name_param id) ->
            on_evar id;
            let loc = to_location id in
            (Labelled "cluster_name", evar ~loc id.node) :: args
      in
      pexp_apply ~loc [%expr Ch_queries.from_select] args
  | F_param { id; alias; final } -> (
      on_evar id;
      let loc = to_location id in
      match final with
      | false ->
          [%expr [%e evar ~loc id.node] ~alias:[%e estring ~loc alias.node]]
      | true ->
          [%expr
            [%e evar ~loc id.node] ~final:true
              ~alias:[%e estring ~loc alias.node]])
  | F_ascribe (from_one, t) ->
      let from_one = stage_from_one ~on_evar from_one in
      let t = stage_typ t in
      [%expr ([%e from_one] : [%t t] Ch_queries.from_one)]

let expand_query ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let query = parse_query ~loc txt in
      stage_query ~on_evar:(Fun.const ()) query
  | _ -> Location.raise_errorf "expected a string literal for the '%%q"

let expand_from ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let from = parse_from ~loc txt in
      [%expr
        Ch_queries.map_from_scope
          [%e stage_from ~on_evar:(Fun.const ()) from]
          (fun [%p from_scope_pattern from] ->
            [%e from_scope_expr ~scopes:[] from])]
  | _ -> Location.raise_errorf "expected a string literal for the '%%f"

let expand_expr ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let expr = parse_expr ~loc txt in
      stage_expr ~on_evar:(Fun.const ()) ~params:[] ~from:From_none expr
  | _ -> Location.raise_errorf "expected a string literal for the '%%e"

let expand_uexpr ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let expr = parse_uexpr ~loc txt in
      stage_expr ~on_evar:(Fun.const ()) ~params:[] ~from:From_none expr
  | _ -> Location.raise_errorf "expected a string literal for the '%%eu"

let expand_typ ~ctxt:_ expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (txt, loc, _)) ->
      let typ = parse_typ ~loc txt in
      stage_typ typ
  | _ -> Location.raise_errorf "expected a string literal for [%%t ...]"

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
      List.map fields ~f:(fun { Syntax.expr; alias } ->
          let expr, typ =
            match expr.node with
            | E_ascribe (expr, typ) -> (expr, typ)
            | _ ->
                Location.raise_errorf ~loc:(to_location expr)
                  "missing type annotation: <expr>::<type>"
          in
          let name =
            match alias with
            | Some alias -> id_to_located alias
            | None ->
                Location.raise_errorf ~loc:(to_location expr)
                  "missing query alias: <expr> AS <alias>"
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

let expand_select ~ctxt:_ (pat : label loc) expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (query, loc, _)) ->
      let query = parse_query ~loc query in
      let fields = extract_typed_fields query in
      let mklabel suffix =
        { pat with txt = Printf.sprintf "%s_%s" pat.txt suffix }
      in
      let type_row =
        let fields =
          List.map fields ~f:(fun (name, typ) ->
              label_declaration ~loc:name.loc ~mutable_:Immutable ~name
                ~type_:(stage_typ_to_ocaml_type typ))
        in
        pstr_type ~loc Nonrecursive
          [
            type_declaration ~loc ~manifest:None ~name:(mklabel "row")
              ~params:[] ~cstrs:[] ~kind:(Ptype_record fields) ~private_:Public;
          ]
      in
      let query, rev_vars =
        let vars = ref [] in
        let on_evar var = vars := var :: !vars in
        let query = stage_query ~on_evar query in
        (query, !vars)
      in
      let parse =
        let init, rest =
          match List.rev fields with
          | init :: fields -> (init, fields)
          | [] -> assert false
        in
        let parse_fields =
          let make (name, typ) =
            let parser = stage_typ_to_parser typ in
            let query =
              stage_expr ~on_evar:(Fun.const ()) ~params:[] ~from:From_none
                Syntax.(make_expr (E_col (make_id "q", make_id name.txt)))
            in
            [%expr Ch_queries.Row.col [%e query] [%e parser]]
          in
          List.fold_left rest ~init:(make init) ~f:(fun acc field ->
              [%expr Ch_queries.Row.( and+ ) [%e make field] [%e acc]])
        in
        let pat_names =
          let make (name, _) = pvar ~loc:name.loc name.txt in
          List.fold_left rest ~init:(make init) ~f:(fun acc field ->
              [%pat? [%p make field], [%p acc]])
        in
        let make =
          let fields =
            List.map fields ~f:(fun (name, _) ->
                let name =
                  Located.mk ~loc:name.loc (Longident.Lident name.txt)
                in
                (name, pexp_ident ~loc:name.loc name))
          in
          pexp_record ~loc fields None
        in
        [%expr
          fun (__q : < q : _ Ch_queries.scope >) ->
            Ch_queries.Row.( let+ ) [%e parse_fields] (fun [%p pat_names] ->
                [%e make])]
      in
      let let_query =
        let query = [%expr fun () -> Ch_queries.query [%e query] [%e parse]] in
        let query =
          List.fold_left rev_vars ~init:query ~f:(fun acc var ->
              let var = id_to_located var in
              let loc = var.loc in
              pexp_fun ~loc (Labelled var.txt) None (ppat_var ~loc var) acc)
        in
        [%stri let [%p ppat_var ~loc pat] = [%e query]]
      in
      pstr_include ~loc
        (include_infos ~loc (pmod_structure ~loc [ type_row; let_query ]))
  | _ ->
      Location.raise_errorf ~loc:expr.pexp_loc
        "expected a string literal for [%%ch.select]"

let select_extension name =
  Extension.V3.declare name Extension.Context.structure_item
    Ast_pattern.(
      pstr
        (pstr_value nonrecursive
           (value_binding ~pat:(ppat_var __') ~expr:__ ^:: nil)
        ^:: nil))
    expand_select

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

let scope_extension name =
  Extension.V3.declare name Extension.Context.pattern
    Ast_pattern.(single_expr_payload __)
    expand_scope

let scope_type_extension name =
  Extension.V3.declare name Extension.Context.core_type
    Ast_pattern.(single_expr_payload __)
    expand_scope_type

let rules =
  [
    Context_free.Rule.extension (query_extension "ch.q");
    Context_free.Rule.extension (query_extension "ch.query");
    Context_free.Rule.extension (from_extension "ch.f");
    Context_free.Rule.extension (from_extension "ch.from");
    Context_free.Rule.extension (expr_extension "ch.e");
    Context_free.Rule.extension (expr_extension "ch.expr");
    Context_free.Rule.extension (typ_extension "ch.t");
    Context_free.Rule.extension (typ_extension "ch.type");
    Context_free.Rule.extension (uexpr_extension "ch.eu");
    Context_free.Rule.extension (uexpr_extension "ch.expr_unsafe");
    Context_free.Rule.extension (select_extension "ch.select");
    Context_free.Rule.extension (scope_extension "ch.scope");
    Context_free.Rule.extension (scope_extension "ch.s");
    Context_free.Rule.extension (scope_type_extension "ch.scope");
    Context_free.Rule.extension (scope_type_extension "ch.s");
  ]

let () = Driver.register_transformation ~rules "queries_ppx"
