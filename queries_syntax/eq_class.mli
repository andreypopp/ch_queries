type !'a t

val equal : 'a Equal.t
val hash : 'a t Hash.t
val hash_fold_t : 'a t Ppx_hash_lib.hash_fold

module Make (H : Hashtbl.HashedType) : sig
  val v : H.t -> H.t t
end
