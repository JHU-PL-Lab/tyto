(**
  SUPER IMPORTANT NOTE:
    It is an invariant in this implementation of concolic evaluation
    that each target is only created once, and therefore we use an
    internal state to attach a unique identifier to each target for
    efficient comparison later (since they are stored in priority
    search queues, they get compared a lot).

    This will break if the concolic evaluator does not have this
    property, and it won't break loudly, so the developer must be
    very careful that this assumption continues to hold. That is,
    the entire concolic evaluation system will be quietly incorrect
    if this property is violated.
*)

type 'k t =
  { path_n : int
  ; priority : int
  ; uniq_id : int
  ; i_env : 'k Interp_common.Input_feeder.t
  ; target_formula : (bool, 'k) Smt.Formula.t
  ; all_formulas : (bool, 'k) Smt.Formula.t list }

val empty : 'k t
(** [empty] is the target with no formulas. i.e. it is unconstrained. *)

val make : (bool, 'k) Smt.Formula.t -> (bool, 'k) Smt.Formula.t list ->
  'k Interp_common.Input_feeder.t -> priority:int -> path_n:int -> 'k t

val compare : 'k t -> 'k t -> int
(** [compare a b] uses the unique identifiers in [a] and [b] to compare,
    and hence only literal equality (of memory location) is sufficient
    for [compare a b] to be [0]. *)
