
open Common

module type DRIVER = sig
  val test_some_program :
    options:Options.t ->
    do_wrap:bool ->
    do_type_splay:Translate.Splay.t ->
    Lang.Ast.some_program ->
    Status.Terminal.t
  (** Performs concolic evaluation on the provided program or times out if the
      timeout limit was exceeded.  The result is printed to stdout. *)

  val test_some_file :
    options:Options.t ->
    do_wrap:bool ->
    do_type_splay:Translate.Splay.t ->
    string ->
    Status.Terminal.t
  (** Performs concolic evaluation on the program in the provided file or times
      out if the timeout limit was exceeded.  The result is printed to stdout. *)

  val eval : Status.Terminal.t Cmdliner.Cmd.t
  (** [eval] can be run with [Cmdliner] to run [test] on the command line arguments. *)
end

module type S = sig
  (* This is generative because a new solver contexts are made *)
  module Make (_ : Target_queue.MAKE) (_ : Evaluator.EVAL) () : DRIVER

  module Eager : DRIVER

  module Deferred : DRIVER

  module Default = Eager

  include DRIVER (* Is Default *)
end

(* No logging at all *)
include S

(* Any derived functions may call the log function (and hence perform effects) *)
module Of_logger (_ : Evaluator.LOGGER) : S
