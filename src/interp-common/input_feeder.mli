
type 'k t

val empty : 'k t

module type S = sig
  module K : Smt.Symbol.KEY
  type nonrec t = K.t t
  val empty : t
  val find : 'a Input.Kind.t -> K.t -> t -> 'a option
  val add : 'a Input.Kind.t -> K.t -> 'a -> t -> t
  val extend : t -> with_:t -> t
  val to_string : t -> string
  val of_model : K.t Smt.Model.t -> t
  val image : t -> Input.t list
  val less_than : t -> K.t -> t
end

module Make : (K : Smt.Symbol.KEY) -> S with module K = K

module Timestamped : module type of Make (Timestamp)
module Indexed : sig
  include module type of Make (Index)
  val of_sequence : Input.t list -> t
end
module Stepped : module type of Make (Step)
