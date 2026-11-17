
type ('a, 'key) t = ('a, 'key) Utils.Separate.t

module type BASE = sig
  include Utils.Comparable.P
  include Smt.Symbol.KEY with type t := t
end

module Make (K : BASE) = struct
  type key = K.t
  include Utils.Separate.Make_with_compare (K)

  let uid = K.uid

  let to_string (type a) (x : a t) : string =
    match x with
    | I k -> Format.sprintf "ikey_$%s" (K.to_string k)
    | B k -> Format.sprintf "bkey_$%s" (K.to_string k)
end

module Stepkey = Make (Step)
module Indexkey = Make (Index)
module Timekey = Make (Timestamp)
