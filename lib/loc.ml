type t = { start_pos : Lexing.position; end_pos : Lexing.position }
(** Location in the source code. *)

type 'a with_loc = { node : 'a; loc : t }
(** Data annotated with its location in the source code. *)
