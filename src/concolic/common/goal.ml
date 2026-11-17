
type 'k t =
  { constraints : (bool, 'k) Smt.Formula.t list
  ; assignments : 'k Interp_common.Input_feeder.t
  }

let empty : 'k t =
  { constraints = []
  ; assignments = Interp_common.Input_feeder.empty
  }

let of_solved_target
  (module Feeder : Interp_common.Input_feeder.S) (target : Feeder.K.t Target.t) model =
  { assignments = Feeder.extend target.i_env ~with_:(Feeder.of_model model)
  ; constraints = target.all_formulas
  }
