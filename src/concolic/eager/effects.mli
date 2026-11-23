
open Interp_common
open Common

(*
  ------------
  MONAD BASICS
  ------------
*)

include Utils.Types.MONAD

(*
  -------------
  STATE AND ENV
  -------------
*)

val fetch : Lang.Ast.Ident.t -> Value.t m
(** [fetch id] is the value in the environment to which [id] maps, or a monadic error
    if [id] is unbound. *)

val local : (Value.Env.t -> Value.Env.t) -> 'a m -> 'a m
(** [local f m] runs [m] under the local environment transformed by [f]. *)

val read : Value.Env.t m
(** [read] is the environment. *)

(*
  --------------
  INTERPRETATION
  --------------
*)

val abort : string -> 'a m
(** [abort msg] aborts with a monadic error with the given reason [msg]. *)

val type_mismatch : string -> 'a m
(** [type_mismatch msg] ends interpretation in a monadic error with the given reason [msg]. *)

val vanish : 'a m
(** [vanish] is a safe monadic exit. *)

val step : Step.t m
(** [step] is the current step count. *)

val incr_step : max_step:Step.t -> unit m
(** [incr_step ~max_step] updates the state and quits if [max_step] has been exceeded. *)

val push_branch : Step.t Direction.t -> unit m
(** [push_branch dir] pushes the [dir] onto the stateful path. *)

val get_input : 'a Input.Kind.t -> default:'a -> Input_feeder.Stepped.t -> Value.t m

val run : 'a m -> Status.Eval.t * (Step.t, Step.t) Path.t * Input_feeder.Stepped.t
(** [run x] is the result of the monad run on an empty initial state and environment. *)
