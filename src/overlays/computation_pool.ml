
open Moonpool

module type COMPUTE_RESULT = sig
  include Utils.Types.MONOID
  val is_signal_to_quit : t -> bool
end

let process_all (module C : COMPUTE_RESULT) (run : (unit -> unit) -> 'a -> C.t)
    (ls : 'a Nel.t) =
  (* We create one thread per work item, but it may be recommended to do fewer
    if this number is huge *)
  let pool = Ws_pool.create ~num_threads:(Nel.length ls) () in
  let is_done = ref false in
  let exception Cancel in
  let yield () =
    if !is_done then raise_notrace Cancel
  in
  let futures = Queue.create () in
  Nel.iter (fun item ->
    Queue.push (Fut.spawn ~on:pool (fun () -> run yield item)) futures
  ) ls;
  let rec go acc =
    match Queue.take_opt futures with
    | None -> acc
    | Some promise when Fut.is_done promise ->
        let acc = C.combine (Fut.await promise) acc in
        if C.is_signal_to_quit acc then
          let () = is_done := true in
          acc
        else
          go acc
    | Some unfinished ->
        let () = Queue.push unfinished futures in
        go acc
  in
  let res = go C.neutral in
  Ws_pool.shutdown pool; (* each item will end when it calls yield *)
  res
