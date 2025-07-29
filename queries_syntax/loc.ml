type t = { start_pos : Lexing.position; end_pos : Lexing.position }
(** Location in the source code. *)

type 'a with_loc = { node : 'a; loc : t }
(** Data annotated with its location in the source code. *)

(** Use when no position is available. *)
let dummy = { start_pos = Lexing.dummy_pos; end_pos = Lexing.dummy_pos }

let with_loc node loc = { node; loc }
let with_dummy_loc node = { node; loc = dummy }
