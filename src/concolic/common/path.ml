
(* the type parameters are a key and an index (when the branch was hit) *)
type ('k, 'i) t = ('k Direction.t * 'i) list

let empty : ('k, 'i) t = []

let cons (e : 'k Direction.t) (i : 'i) (l : ('k, 'i)  t) : ('k, 'i) t =
  (e, i) :: l

let to_dirs (l : ('k, 'i) t) : ('k Direction.t * 'i) list =
  List.rev l

let length = List.length

let find_all (f : 'i -> bool) (l : ('k, 'i) t) : ('k, 'i) t =
  List.filter (fun (_, i) -> f i) l

let map_i (f: 'i -> 'ii) (l : ('k, 'i) t) : ('k, 'ii) t =
  List.map (fun (a, i) -> a, f i) l
