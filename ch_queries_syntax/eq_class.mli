(** Equivalence class. *)

type !'a t
(** An equivalence class of values of type ['a]. Values of such types are fast
    to compare (equal values of type ['a] are mapped to the same equivalence
    class, so comparison is just a pointer comparison) and fast to hash
    (precomputed). *)

val equal : 'a t Equal.t
val hash : 'a t Hash.t
val hash_fold_t : 'a t Ppx_hash_lib.hash_fold

(** Create an equivalence class for a hashable/comparable type. *)
module Make (H : Hashtbl.HashedType) : sig
  val v : H.t -> H.t t
  (** [v x] is an equivalence class for the value [x]. *)
end
