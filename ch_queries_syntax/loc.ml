(** Location in the source code. *)

type position = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}

type t = { start_pos : position; end_pos : position }
(** Location in the source code. *)

(** Use when no position is available. *)
let dummy = { start_pos = Lexing.dummy_pos; end_pos = Lexing.dummy_pos }
