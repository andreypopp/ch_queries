(** Lexer for unsafe expressions. *)

val token : unit -> Lexing.lexbuf -> Uparser.token
(** tokenize input, raises [Error] on error *)

exception Error of string

val string_of_token : Uparser.token -> string
val tokenize_debug : string -> unit
