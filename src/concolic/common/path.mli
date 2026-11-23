
type ('k, 'i) t
(** [('k ,'i) t] is a concolic path, where symbolic formulas are keyed by ['k],
  and there is an index type ['i]. *)

val empty : ('k, 'i) t
(** [empty] is the path of length [0]. *)

val cons : 'k Direction.t -> 'i -> ('k, 'i) t -> ('k, 'i) t
(** [cons dir path] puts [dir] as the most recent direction taken after [path]. *)

val to_dirs : ('k, 'i) t -> ('k Direction.t * 'i) list
(** [to_dirs path] is the list of directions in the order they were consed onto [path]. *)

val length : ('k, 'i) t -> int
(** [length path] is the number of directions in [path], in constant time. *)

val find_all : ('i -> bool) -> ('k, 'i) t -> ('k, 'i) t

val map_i : ('i -> 'ii) -> ('k, 'i) t -> ('k, 'ii) t
