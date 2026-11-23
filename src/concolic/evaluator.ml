
open Interp_common
open Common

module type LOGGER = sig
  val log : Stat.t -> unit
end

(*
  Evaluates an expression to a concolic path.

  Note:
  - If we want to collect a log from this evaluation, then we would want a functor that passes in the log
    we're using because that type would have to line up with the builder from `Log` in the `Make` functor
    below.
  - Or this type is parametrized by the log type, which would not be a monad but would be fully run and
    returned here. Then the loop just uses `tell` to collect that whole log in.
*)
type 'key eval = Lang.Ast.Embedded.t -> 'key Input_feeder.t
  -> max_step:Step.t -> default_input:('a. 'a Input.Kind.t -> 'a)
  -> Status.Eval.t * ('key, 'key) Path.t * 'key Input_feeder.t

module type EVAL = sig
  type key
  val eval : key eval
end

(*
  -------------
  CONCOLIC LOOP
  -------------
*)

module type KEY = sig
  include Smt.Symbol.KEY
  val compare : t -> t -> int
end

module Make (K : KEY) (Make_tq : Target_queue.MAKE) (Logger : LOGGER) = struct
  (* module TKey = Step Keys for formulas in targets *)
  type key = K.t
  module Tq = Make_tq (K)
  module Feeder = Input_feeder.Make (K)

  let adjacent_targets ~(max_tree_depth : int) (target : key Target.t)
      (final_path : (key, K.t) Path.t) (i_env : Feeder.t)
      : key Target.t list * is_pruned:bool =
    let rec make acc_depth acc_formulas = function
      | [] -> [], ~is_pruned:false
      | _ when acc_depth > max_tree_depth -> [], ~is_pruned:true
      | ((dir, _), priority) :: tl ->
        let depth = acc_depth + 1 in
        let new_targets =
          List.map (fun e ->
            Target.make e acc_formulas i_env ~priority ~path_n:depth
          ) (Direction.negations dir)
        and ret_targets, ~is_pruned =
          make depth (Direction.to_formula dir :: acc_formulas) tl
        in
        List.rev_append new_targets ret_targets, ~is_pruned
    in
    let dirs =
      Path.to_dirs final_path
      |> List.mapi (fun i x -> x, i) (* index in original path becomes priority *)
      |> List.sort (fun ((_, k), _) ((_, k'), _) -> K.compare k k')
    in
    let stem = List.drop target.path_n dirs in
    make target.path_n target.all_formulas stem

  (*
    Falls back on all-zero input feeder on first run and default (random) feeder after that.
  *)
  let c_loop_body (e : Lang.Ast.Embedded.t) (eval : key eval) (tq : Tq.t)
    (solve : key Smt.Solve.solver) ~(max_tree_depth : int)
    ~(max_step : Step.t) : Status.Terminal.t =
    let is_first_interp = ref true in
    let rec loop tq =
      let () = Utils.Time.yield_to_timer () in
      match Tq.pop tq with
      | Some (target, tq) -> begin
          let solve_span, solve_result = Utils.Time.time solve target.target_formula in
          let () = Logger.log @@ Time (Solve_time, solve_span) in
          let () = Logger.log @@ Count (N_solves, 1) in
          let () = Utils.Time.yield_to_timer () in
          match solve_result with
          | Sat model -> loop_on_model target tq model
          | Unknown -> Status.min Unknown (loop tq)
          | Unsat -> loop tq
        end
      | None -> Status.Exhausted_full_tree

    and loop_on_model target tq model =
      let goal = Goal.of_solved_target (module Feeder) target model in
      let default_input =
        if !is_first_interp then Input.Kind.zero else Input.Kind.rand
      in
      let () = is_first_interp := false in
      let interp_span, (status, path, ienv) =
        Utils.Time.time (eval e ~max_step ~default_input) goal.assignments
      in
      let () = Logger.log @@ Time (Interp_time, interp_span) in
      let () = Logger.log @@ Count (N_interps, 1) in
      let k ~reached_max_step =
        let targets, ~is_pruned =
          adjacent_targets target path ~max_tree_depth ienv
        in
        let a = loop (Tq.push_list tq targets) in
        if is_pruned || reached_max_step then
          Status.min Exhausted_pruned_tree a
        else
          a
      in
      match status with
      | (Found_abort _ | Type_mismatch _ | Unbound_variable _) as s ->
        let () = Logger.log @@ Depth (Target_depth, target.path_n) in
        let () = Logger.log @@ Depth (Target_priority, target.priority) in
        let () = Logger.log @@ Depth (Error_depth, Path.length path) in
        s
      | Finished -> k ~reached_max_step:false (* TODO: track number of vanishes *)
      | Reached_max_step -> k ~reached_max_step:true

    in
    let span, res = Utils.Time.time loop tq in
    let () = Logger.log @@ Time (Total_time, span) in
    res

  let c_loop ~(options : Options.t) (eval : K.t eval) (solve : key Smt.Solve.solver)
      (e : Lang.Ast.Embedded.t) : Status.Terminal.t =
    if options.is_random then Random.self_init () else Rand.reset ();
    let res =
      Utils.Time.with_timeout options.global_timeout @@ fun () ->
        let empty_tq = Tq.make options in
        c_loop_body
          e
          eval
          (Tq.push_list empty_tq [ Target.empty ])
          solve
          ~max_tree_depth:options.max_tree_depth
          ~max_step:(Step options.global_max_step)
    in
    match res with
    | Ok a -> a
    | Error _ -> Status.Timeout
end
