
module type KEY = sig
  type t
  val uid : t -> Utils.Uid.t
end

(* Symbols have a phantom 'k (key) parameter. The underlying key is actually a
  UID. *)
type ('a, 'k) t = ('a, Utils.Uid.t) Utils.Separate.t

let compare (type a) (x : (a, 'k) t) (y : (a, 'k) t) : int =
  match x, y with
  | I xi, I yi
  | B xi, B yi -> Utils.Uid.compare xi yi

let equal (type a) (x : (a, 'k) t) (y : (a, 'k) t) : bool =
  match x, y with
  | I xi, I yi
  | B xi, B yi -> xi = yi

let make_int (k : 'k) (uid : 'k -> Utils.Uid.t) : (int, 'k) t =
  I (uid k)

let make_bool (k : 'k) (uid : 'k -> Utils.Uid.t) : (bool, 'k) t =
  B (uid k)

module Make (Key : KEY) = struct
  type nonrec 'a t = ('a, Key.t) t

  let make_int (k : Key.t) : int t =
    I (Key.uid k)

  let make_bool (k : Key.t) : bool t =
    B (Key.uid k)
end
