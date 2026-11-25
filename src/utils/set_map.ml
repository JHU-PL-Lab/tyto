
(* Make weight-balanced set and map modules. *)
module Make_W (K : Baby.OrderedType) = struct
  module B = Baby.W.Make (K)

  module Map = struct
    include B.Map
    let domain = B.domain

    let extend (base : 'a t) ~(with_ : 'a t) : 'a t =
      union (fun _ _ v -> Some v) base with_

    let list_map (f : key -> 'a -> 'b) (t : 'a t) : 'b list =
      let rec aux enum =
        match Enum.head_opt enum with
        | Some (k, a) -> f k a :: aux (Enum.tail enum)
        | None -> []
      in
      aux (Enum.enum t)
  end

  module Set = struct
    include B.Set

    let list_map (f : elt -> 'b) (t : t) : 'b list =
      let rec aux enum =
        match Enum.head_opt enum with
        | Some a -> f a :: aux (Enum.tail enum)
        | None -> []
      in
      aux (Enum.enum t)
  end
end
