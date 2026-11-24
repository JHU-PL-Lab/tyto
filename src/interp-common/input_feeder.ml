
(*
  We will use a zipper map that has fast reads and writes nearest the last read
  or write because inputs are very often read and written in increasing order.
  Inputs are keyed by how many steps the program has taken, which is increasing.
  It only takes exactly one traversal to reset at the start of the program, and
  otherwise the actions are cheap.
*)
module UMap = Utils.Uid.Map

(* phantom key type *)
type 'k t = Input.t UMap.t

let empty : 'k. 'k t = UMap.empty

module type S = sig
  module K : Smt.Symbol.KEY
  type nonrec t = K.t t
  val empty : t
  val find : 'a Input.Kind.t -> K.t -> t -> 'a option
  val add : 'a Input.Kind.t -> K.t -> 'a -> t -> t
  val extend : t -> with_:t -> t
  val to_string : t -> string
  val of_model : K.t Smt.Model.t -> t
  val image : t -> Input.t list
  val less_than : t -> K.t -> t
end

module Make (K : Smt.Symbol.KEY) = struct
  module K = K
  type nonrec t = K.t t

  let empty : t = UMap.empty

  (* Hides failing extraction because input environments might be reused. *)
  let find (type a) (kind : a Input.Kind.t) (key : K.t) (m : t) : a option =
    Option.bind (UMap.find_opt (K.uid key) m) (Input.extract_opt kind)

  let add (type a) (kind : a Input.Kind.t) (key : K.t) (input : a) (m : t) : t =
    UMap.add (K.uid key) (
      match kind with
      | Input.Kind.KBool -> Input.IBool input
      | KInt -> IInt input
    ) m

  let extend = UMap.extend

  let to_string (m : t) : string =
    let make_mapping (uid, input) =
      Printf.sprintf "%d |-> %s" (Utils.Uid.to_int uid) (Input.to_string input)
    in
    let body =
      m
      |> UMap.to_list
      |> List.map make_mapping
      |> String.concat " ; "
    in
    Printf.sprintf "{ %s }" body

  let of_model (model : K.t Smt.Model.t) : t =
    List.fold_left (fun acc uid ->
      let v =
        match model.value (I uid) with
        | Some i -> Input.IInt i
        | None -> IBool (Option.get (model.value (B uid)))
      in
      UMap.add uid v acc
    ) empty model.domain

  let image m =
    List.map snd (UMap.to_list m)

  let less_than m k =
    let lt, _eq_opt, _gt = UMap.split (K.uid k) m in
    lt
end

module Timestamped = Make (Timestamp)

module Indexed = struct
  include Make (Index)

  let of_sequence ls =
    fst @@
    List.fold_left (fun (acc, idx) input ->
      match input with
      | Input.IInt i ->
        add Input.Kind.KInt idx i acc, idx + 1
      | IBool b ->
        add KBool idx b acc, idx + 1
    ) (empty, 0) ls
end

module Stepped = Make (Step)
