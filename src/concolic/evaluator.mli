
open Interp_common
open Common

module type LOGGER = sig
  val log : Stat.t -> unit
end

type 'key eval = Lang.Ast.Embedded.t -> 'key Input_feeder.t
  -> max_step:Step.t -> default_input:('a. 'a Input.Kind.t -> 'a)
  -> Status.Eval.t * ('key, 'key) Path.t * 'key Input_feeder.t
(** [eval] is the type of functions that concolically evaluate programs to a path
    of concolic formulas keyed by ['key]. *)

module type EVAL = sig
  type key
  val eval : key eval
end

module type KEY = sig
  include Smt.Symbol.KEY
  val compare : t -> t -> int
end

(*
  Here, we have passed in a logger that has already been transformed with the P monad.
  This is in contrast to how we pass in a key and a functor to make a target queue from that key.
  The parallel would be to pass in a log transformer.
  (But I should just pass in a made target queue and pull the k from that!)
*)
module Make : functor (K : KEY) (_ : Target_queue.MAKE) (_ : LOGGER) -> sig
  val c_loop : options:Options.t -> K.t eval -> K.t Smt.Solve.solver
    -> Lang.Ast.Embedded.t -> Status.Terminal.t
  (** [c_loop ~options eval pgm] is the result of concolic looping on [pgm] using
      the concolic evaluation function [eval]. Performs effects from the logger. *)
end
