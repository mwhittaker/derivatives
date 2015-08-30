type t
val null : t
val epsilon : t
val char: char -> t
val cat: t -> t -> t
val star: t -> t
val either: t -> t -> t
val both: t -> t -> t
val not: t -> t

module Infix : sig
  val (++) : t -> t -> t (* cat *)
  val (||) : t -> t -> t (* either *)
  val (&&) : t -> t -> t (* both *)
  val (!)  : t -> t      (* not *)
end

module Chars : sig
  val a: t
  val b: t
  val c: t
  val d: t
  val e: t
  val f: t
  val g: t
  val h: t
  val i: t
  val j: t
  val k: t
  val l: t
  val m: t
  val n: t
  val o: t
  val p: t
  val q: t
  val r: t
  val s: t
  val t: t
  val u: t
  val v: t
  val w: t
  val x: t
  val y: t
  val z: t
end

val show : t -> string
val dc : char -> t -> t
val d: string -> t -> t
val v: t -> t
val matches : t -> string -> bool
