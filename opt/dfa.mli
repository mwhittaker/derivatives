(* Deterministic Finite State Machine *)
type t
val to_string: t -> string
val from_re: Re.t -> t
val matches: t -> string -> bool
