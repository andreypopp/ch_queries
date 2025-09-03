type !'a t = { hash : int }

let equal a b = Equal.physical a b
let compare a b = Int.compare a.hash b.hash
let hash a = a.hash
let hash_fold_t s a = Ppx_hash_lib.Std.Hash.fold_int s a.hash

module Make (H : Hashtbl.HashedType) : sig
  val v : H.t -> H.t t
end = struct
  module Items = Ephemeron.K1.Make (H)

  let items = Items.create 1023

  let v x =
    match Items.find items x with
    | exception Not_found ->
        let item = { hash = H.hash x } in
        Items.add items x item;
        item
    | eq -> eq
end
