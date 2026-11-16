
type t = Step of int [@@unboxed]

let equal (Step a) (Step b) = a = b
let compare (Step a) (Step b) = Int.compare a b

let zero = Step 0

let[@inline always] next (Step i) = Step (i + 1)

let[@inline always] to_int (Step i) = i

let uid (Step i) = Utils.Uid.of_int i

let to_string (Step i) = Int.to_string i
