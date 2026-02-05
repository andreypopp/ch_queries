type !'a t = {
  tag : int;
  hash : int;
  x : 'a;  (** keep a strong reference to [x] *)
}
[@@ocaml.warning "-69"]

let equal a b = Equal.physical a b
let compare a b = Int.compare a.tag b.tag
let hash a = a.hash
let hash_fold_t s a = Ppx_hash_lib.Std.Hash.fold_int s a.hash

module Make (H : Hashtbl.HashedType) : sig
  val v : H.t -> H.t t
end = struct
  module Items = Ephemeron.K1.Make (H)

  let items : H.t t Items.t = Items.create 1023

  let gentag =
    let ref = ref 0 in
    fun () -> Ref.get_then_incr ref

  let v x =
    match Items.find items x with
    | exception Not_found ->
        let item = { tag = gentag (); hash = H.hash x; x } in
        Items.add items x item;
        item
    | eq -> eq
end
