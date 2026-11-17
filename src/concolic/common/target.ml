
(* we can safely use this when making because it's promised that we never make a target twice *)
let uid = Utils.Counter.create ()

type 'k t =
  { path_n : int
  ; uniq_id : int
  ; i_env : 'k Interp_common.Input_feeder.t
  ; target_formula : (bool, 'k) Smt.Formula.t
  ; all_formulas : (bool, 'k) Smt.Formula.t list }

let initial_id = Utils.Counter.next uid

let empty : 'k t =
  { path_n = 0
  ; uniq_id = initial_id
  ; i_env = Interp_common.Input_feeder.empty
  ; target_formula = Smt.Formula.trivial
  ; all_formulas = []
  }

let make (last_formula : (bool, 'k) Smt.Formula.t)
  (other_formulas : (bool, 'k) Smt.Formula.t list)
  (i_env : 'k Interp_common.Input_feeder.t)
  ~(priority : int) : 'k t =
  let target_formula = Smt.Formula.scc last_formula ~wrt:other_formulas
  and all_formulas = last_formula :: other_formulas
  and uniq_id = Utils.Counter.next uid
  and path_n = priority in
  { target_formula ; all_formulas ; i_env ; uniq_id ; path_n }

(*
  SUPER IMPORTANT NOTE:
  * The concolic evaluator is currently built to only make each
    target once. Therefore, we can create a unique id upon target
    creation, and that is safe to use when comparing.
  * This will break if the concolic evaluator does not have this
    property, and it won't break loudly, so the developer must be
    very careful that this assumption continues to hold.
  * If only equality were needed, we would use `a == b`, as it
    does the same.
*)
let compare (a : 'k t) (b : 'k t) : int =
  Int.compare a.uniq_id b.uniq_id

let path_n ({ path_n ; _ } : 'k t) : int =
  path_n
