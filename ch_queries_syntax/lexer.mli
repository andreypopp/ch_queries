(** Lexer. *)

val token : Lexing.lexbuf -> Parser.token
(** tokenize input, raises [Error] on error *)

exception Error of string

val string_of_token : Parser.token -> string
val tokenize_debug : string -> unit
