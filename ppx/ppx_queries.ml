open Ppxlib
open Ast_builder.Default

let parse_query_string ~loc s =
  let lexbuf = Lexing.from_string s in
  Lexing.set_position lexbuf
    {
      loc.Location.loc_start with
      pos_cnum =
        loc.Location.loc_start.pos_cnum
        +
        (* for openning quote, but breaks in case of "{|", this is a TODO *)
        1;
    };
  try Queries.Parser.a_query Queries.Lexer.token lexbuf
  with exn ->
    let msg =
      Printf.sprintf "Failed to parse query: %s" (Printexc.to_string exn)
    in
    Location.Error.raise (Location.Error.make ~loc:Location.none ~sub:[] msg)

let parse_expr_string s =
  let lexbuf = Lexing.from_string s in
  try Queries.Parser.a_expr Queries.Lexer.token lexbuf
  with exn ->
    let msg =
      Printf.sprintf "Failed to parse expression: %s" (Printexc.to_string exn)
    in
    Location.Error.raise (Location.Error.make ~loc:Location.none ~sub:[] msg)

let loc_to_location ({ start_pos; end_pos } : Queries.Loc.t) : location =
  { loc_start = start_pos; loc_end = end_pos; loc_ghost = false }

let rec from_scope_expr from =
  let open Queries.Syntax in
  let loc = loc_to_location from.Queries.Loc.loc in
  match from.Queries.Loc.node with
  | F from_one -> from_one_scope_expr from_one
  | F_join { from; join; _ } ->
      let x = from_scope_expr from in
      let y = from_one_scope_expr join in
      [%expr [%e x], [%e y]]

and from_one_scope_expr from_one =
  let open Queries.Syntax in
  let loc = loc_to_location from_one.Queries.Loc.loc in
  match from_one.Queries.Loc.node with
  | F_table { alias; _ } | F_select { alias; _ } | F_value { alias; _ } ->
      evar ~loc alias.node

let rec stage_expr ~from expr =
  let loc = loc_to_location expr.Queries.Loc.loc in
  match expr.Queries.Loc.node with
  | Queries.Syntax.E_col (scope, id) ->
      let e = evar ~loc scope.Queries.Loc.node in
      let e' = pexp_send ~loc e (Located.mk ~loc "query") in
      let p = pvar ~loc scope.Queries.Loc.node in
      [%expr
        [%e e'] (fun [%p p] ->
            [%e pexp_send ~loc e (Located.mk ~loc id.Queries.Loc.node)])]
  | Queries.Syntax.E_lit (L_int n) -> [%expr Queries.Expr.int [%e eint ~loc n]]
  | Queries.Syntax.E_lit (L_bool b) ->
      [%expr Queries.Expr.bool [%e ebool ~loc b]]
  | Queries.Syntax.E_lit (L_string s) ->
      [%expr Queries.Expr.string [%e estring ~loc s]]
  | Queries.Syntax.E_call (name, args) -> (
      let args_expr = List.map (stage_expr ~from) args in
      match name.Queries.Loc.node with
      | "OR" -> eapply ~loc [%expr Queries.Expr.( || )] args_expr
      | "AND" -> eapply ~loc [%expr Queries.Expr.( && )] args_expr
      | _ ->
          let f = "Queries.Expr." ^ name.Queries.Loc.node in
          eapply ~loc (evar ~loc f) args_expr)
  | Queries.Syntax.E_value var -> (
      let e = evar ~loc var.Queries.Loc.node in
      match Option.map from_scope_expr from with
      | None -> e
      | Some arg -> [%expr [%e e] [%e arg]])

let stage_field ~from idx { Queries.Syntax.expr; alias } =
  let idx = idx + 1 in
  let loc = loc_to_location expr.Queries.Loc.loc in
  let name =
    let name, loc =
      match alias with
      | Some name ->
          let loc = loc_to_location name.Queries.Loc.loc in
          (name.Queries.Loc.node, loc)
      | None -> ("_" ^ string_of_int idx, loc)
    in
    Located.mk ~loc name
  in
  pcf_method ~loc (name, Public, Cfk_concrete (Fresh, stage_expr ~from expr))

let rec from_scope_pattern ?kind from =
  let open Queries.Syntax in
  let loc = loc_to_location from.Queries.Loc.loc in
  match from.Queries.Loc.node with
  | F from_one ->
      let p = from_one_scope_pattern from_one in
      [%pat? ([%p p] : _ Queries.scope)]
  | F_join { from; join; kind = kind'; _ } ->
      let x = from_scope_pattern from in
      let y =
        let kind = Option.value kind ~default:kind' in
        match kind with
        | `INNER_JOIN ->
            [%pat? ([%p from_one_scope_pattern join] : _ Queries.scope)]
        | `LEFT_JOIN ->
            [%pat?
              ([%p from_one_scope_pattern join] : _ Queries.nullable_scope)]
      in
      [%pat? [%p x], [%p y]]

and from_one_scope_pattern from_one =
  let open Queries.Syntax in
  let loc = loc_to_location from_one.Queries.Loc.loc in
  match from_one.Queries.Loc.node with
  | F_table { alias; _ } | F_select { alias; _ } | F_value { alias; _ } ->
      pvar ~loc alias.node

let rec stage_query
    { Queries.Loc.node = { Queries.Syntax.fields; from; where }; loc } =
  let loc = loc_to_location loc in
  let select =
    let select_methods = List.mapi (stage_field ~from:(Some from)) fields in
    let select_obj =
      pexp_object ~loc
        { pcstr_self = ppat_any ~loc; pcstr_fields = select_methods }
    in
    pexp_fun ~loc Nolabel None (from_scope_pattern from) select_obj
  in
  let args =
    [ (Labelled "select", select); (Labelled "from", stage_from from) ]
  in
  let args =
    match where with
    | None -> args
    | Some where ->
        let loc = loc_to_location where.Queries.Loc.loc in
        let where_fun =
          pexp_fun ~loc Nolabel None (from_scope_pattern from)
            (stage_expr ~from:(Some from) where)
        in
        (Labelled "where", where_fun) :: args
  in
  pexp_apply ~loc [%expr Queries.select] ((Nolabel, [%expr ()]) :: List.rev args)

and stage_from from =
  let open Queries.Syntax in
  let loc = loc_to_location from.Queries.Loc.loc in
  match from.Queries.Loc.node with
  | F from_one -> [%expr Queries.from [%e stage_from_one from_one]]
  | F_join { kind; from = base; join; on } ->
      let f =
        match kind with
        | `INNER_JOIN -> [%expr Queries.join]
        | `LEFT_JOIN -> [%expr Queries.left_join]
      in
      let on =
        pexp_fun ~loc Nolabel None
          (from_scope_pattern ~kind:`INNER_JOIN from)
          (stage_expr ~from:(Some from) on)
      in
      [%expr [%e f] [%e stage_from base] [%e stage_from_one join] ~on:[%e on]]

and stage_from_one from_one =
  let open Queries.Syntax in
  let loc = loc_to_location from_one.Queries.Loc.loc in
  match from_one.Queries.Loc.node with
  | F_table { db; table; alias } ->
      let qname =
        Printf.sprintf "Database.%s.%s"
          (String.capitalize_ascii db.node)
          table.node
      in
      [%expr [%e evar ~loc qname] ~alias:[%e estring ~loc alias.node]]
  | F_select { select; alias } ->
      let select_expr = stage_query select in
      let alias_expr = estring ~loc alias.node in
      [%expr [%e select_expr] ~alias:[%e alias_expr]]
  | F_value { id; alias } ->
      let loc = loc_to_location id.loc in
      [%expr [%e evar ~loc id.node] ~alias:[%e estring ~loc alias.node]]

let expand_select ~ctxt:_ query =
  let query = parse_query_string ~loc:query.Loc.loc query.Loc.txt in
  stage_query query

let expand_expr ~ctxt:_ expr_str =
  let expr = parse_expr_string expr_str in
  stage_expr ~from:None expr

let select_extension =
  Extension.V3.declare "query" Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __'))
    expand_select

let expr_extension =
  Extension.V3.declare "expr" Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand_expr

let expr_structure_extension =
  Extension.V3.declare "expr" Extension.Context.structure_item
    Ast_pattern.(pstr (pstr_value __ (__ ^:: nil) ^:: nil))
    (fun ~ctxt rec_flag vb ->
      let loc = Expansion_context.Extension.extension_point_loc ctxt in
      (* Transform the expression recursively to find string literals *)
      let rec transform_expr expr =
        match expr.pexp_desc with
        | Pexp_constant (Pconst_string (expr_str, _, _)) ->
            let expr = parse_expr_string expr_str in
            stage_expr ~from:None expr
        | Pexp_function ([ param ], constraint_opt, Pfunction_body expr) ->
            {
              expr with
              pexp_desc =
                Pexp_function
                  ( [ param ],
                    constraint_opt,
                    Pfunction_body (transform_expr expr) );
            }
        | _ ->
            Location.raise_errorf
              "expected a string literal or a function with a single argument"
      in
      let transformed_vb = { vb with pvb_expr = transform_expr vb.pvb_expr } in
      pstr_value ~loc rec_flag [ transformed_vb ])

let rules =
  [
    Context_free.Rule.extension select_extension;
    Context_free.Rule.extension expr_extension;
    Context_free.Rule.extension expr_structure_extension;
  ]

let () = Driver.register_transformation ~rules "ppx_queries"
