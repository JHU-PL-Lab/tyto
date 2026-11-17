
open Common

module type LOGGER = sig
  val log : Stat.t -> unit
end

type eval = Lang.Ast.Embedded.t -> Interp_common.Input_feeder.Stepped.t
  -> max_step:Interp_common.Step.t
  -> default_input:('a. 'a Interp_common.Input.Kind.t -> 'a)
  -> Status.Eval.t * Interp_common.Step.t Path.t * Interp_common.Input_feeder.Stepped.t
(** [eval] is the type of functions that concolically evaluate programs to a path
    of concolic formulas keyed by steps. *)

module type EVAL = sig
  val eval : eval
end

(*
  Here, we have passed in a logger that has already been transformed with the P monad.
  This is in contrast to how we pass in a key and a functor to make a target queue from that key.
  The parallel would be to pass in a log transformer.
  (But I should just pass in a made target queue and pull the k from that!)
*)
module Make : functor (_ : Target_queue.MAKE) (_ : LOGGER) -> sig
  val c_loop : options:Options.t -> eval -> Interp_common.Step.t Smt.Solve.solver
    -> Lang.Ast.Embedded.t -> Status.Terminal.t
  (** [c_loop ~options eval pgm] is the result of concolic looping on [pgm] using
      the concolic evaluation function [eval]. Performs effects from the logger. *)
end
