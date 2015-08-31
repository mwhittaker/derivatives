(* Extended regular expressions type and combinators. *)
type t
val null    : t           (* ∅     *)
val epsilon : t           (* ε     *)
val char    : char -> t   (* a     *)
val cat     : t -> t -> t (* r·s   *)
val star    : t -> t      (* r*    *)
val either  : t -> t -> t (* (r|s) *)
val both    : t -> t -> t (* (r&s) *)
val not     : t -> t      (* ¬r    *)

(* Infix version of regular expressions combinators. *)
module Infix : sig
  val (++) : t -> t -> t (* cat    *)
  val (||) : t -> t -> t (* either *)
  val (&&) : t -> t -> t (* both   *)
  val (!)  : t -> t      (* not    *)
end

(* Regular expressions matching single letters. *)
module Chars : sig
  val a: t val b: t val c: t val d: t val e: t
  val f: t val g: t val h: t val i: t val j: t
  val k: t val l: t val m: t val n: t val o: t
  val p: t val q: t val r: t val s: t val t: t
  val u: t val v: t val w: t val x: t val y: t
  val z: t
end

(* Converts a regular expression to a pretty string. *)
val to_string : t -> string

(* ν r = ε if r accepts ε, and ∅ otherwise. *)
val v: t -> t

(* dc a r is ∂ₐ r: the (character) derivative of r with respect to a. *)
val dc : char -> t -> t

(* d m r is ∂ₘ r: the derivative of r with respect to m. *)
val d: string -> t -> t

(* matches r u or (r ~ u) evaluates to true iff r accepts u. *)
val matches : t -> string -> bool
