
include Concolic.Deferred.Dvalue.Make (Utils.Identity)

module Without_symbols = Lang.Value.Embedded (Utils.Identity)
module Dmain = Concolic.Deferred.Main.Make (struct
  include Interp_common.Timestamp
  let choose time _step = time
end)
module V = Dmain.Effects.Value

(*
  Values cannot be recursive, so this will terminate.
  I am not worrying about performance. This will recompute the same value many
  times in case the symbol shows up in several spots.
*)
let of_concolic (v : V.whnf) (m : V.Symbol_map.t) : Without_symbols.t =
  let rec subst (v : V.t) : Without_symbols.t =
    match v with
    | VSymbol t ->
      Concolic.Deferred.Time_map.find t m
      |> V.cast_up
      |> subst
    (* Nothing to do *)
    | VUnit -> VUnit
    | VInt (i, _) -> VInt i
    | VBool (b, _) -> VBool b
    (* Homomorphic *)
    | VVariant { label ; payload } -> VVariant { label ; payload = subst payload }
    | VRecord record_body -> VRecord (Lang.Ast.RecordLabel.Map.map subst record_body)
    | VModule module_body -> VModule (Lang.Ast.RecordLabel.Map.map subst module_body)
    | VUntouchable v -> VUntouchable (subst v)
    (* Expressions FIXME : subst into env *)
    | VFunClosure { param ; closure = { env = _ ; body } } ->
      VFunClosure { param ; closure = { env = Without_symbols.Env.empty ; body } }
  in
  subst (V.cast_up v)