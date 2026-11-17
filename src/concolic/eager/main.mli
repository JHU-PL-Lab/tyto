
open Common
open Interp_common

val eager_eval : Lang.Ast.Embedded.t -> Step.t Input_feeder.t -> max_step:Step.t
  -> default_input:('a. 'a Interp_common.Input.Kind.t -> 'a)
  -> Status.Eval.t * Step.t Path.t * Step.t Input_feeder.t
(** [eager_eval] is eager concolic evaluation with standard eager semantics. *)
