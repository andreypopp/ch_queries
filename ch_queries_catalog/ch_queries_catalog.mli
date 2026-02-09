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
end

type catalog

val create_catalog : unit -> catalog

type ('params, 'scope) a_column =
  | A_column :
      ('n, 's, 't) Parse.t * ('params -> 'scope -> ('n, 's) expr)
      -> ('params, 'scope) a_column

val a_column :
  ('n, 's, 't) Parse.t ->
  ('params -> 'scope -> ('n, 's) expr) ->
  ('params, 'scope) a_column

type view

val register_view :
  catalog ->
  db:string ->
  name:string ->
  params:'params Params.t ->
  from:('params -> 'scope from) ->
  select:(string * ('params, 'scope) a_column) list ->
  ?prewhere:('params -> 'scope -> (_, bool) expr) ->
  ?where:('params -> 'scope -> (_, bool) expr) ->
  ?group_by:('params -> 'scope -> a_expr list) ->
  ?having:('params -> 'scope -> (_, bool) expr) ->
  unit ->
  unit

val rewrite_query : catalog -> Syntax.query -> Syntax.query
val rewrite_query_to_string : catalog -> string -> string
