open Core.Std

(* Extended regular expressions type. It is an invariant that all lists have at
 * least two elements. *)
type t =
  | Null
  | Epsilon
  | Char of char
  | Cat of t list
  | Star of t
  | Or of t list
  | And of t list
  | Not of t

let null =
  Null

let epsilon =
  Epsilon

let char c =
  Char c

let cat r s =
  match r, s with
  | Null, _ -> Null             (*     ∅·r ≈ ∅       *)
  | _, Null -> Null             (*     r·∅ ≈ ∅       *)
  | Epsilon, r -> r             (*     ε·r ≈ ε       *)
  | r, Epsilon -> r             (*     r·ε ≈ ε       *)
  | Cat rs, s -> Cat (rs @ [s]) (* (r·s)·t ≈ r·(s·t) *)
  | s, Cat rs -> Cat (s::rs)    (* (r·s)·t ≈ r·(s·t) *)
  | _ -> Cat [r; s]

let star r =
  match r with
  | Star r -> Star r   (* ( r* )* ≈ r *)
  | Epsilon -> Epsilon (*      ε* ≈ ε *)
  | Null -> Epsilon    (*      ∅* ≈ ε *)
  | _ -> Star r

let either r s =
  match r, s with
  | Not Null, _ -> Not Null                         (*      ¬∅ + r ≈ ¬∅          *)
  | _, Not Null -> Not Null                         (*      ¬∅ + r ≈ ¬∅          *)
  | Null, r -> r                                    (*       ∅ + r ≈ ∅           *)
  | r, Null -> r                                    (*       ∅ + r ≈ ∅           *)
  | Or rs, r
  | r, Or rs when List.mem rs r -> Or rs            (*       r + r ≈ r           *)
  | r, Or rs
  | Or rs, r -> Or (List.merge rs [r] ~cmp:compare) (*       r + s ≈ s + r
                                                       (r + s) + t ≈ r + (s + t) *)
  | _ -> Or (List.sort ~cmp:compare [r; s])

let both r s =
  match r, s with
  | Null, _ -> Null                                   (*       ∅ & r ≈ ∅           *)
  | _, Null -> Null                                   (*       ∅ & r ≈ ∅           *)
  | Not Null, r -> r                                  (*      ¬∅ & r ≈ ∅           *)
  | r, Not Null -> r                                  (*      ¬∅ & r ≈ ∅           *)
  | r, And rs
  | And rs, r when List.mem rs r -> And rs            (*       r & r ≈ r           *)
  | r, And rs
  | And rs, r -> And (List.merge rs [r] ~cmp:compare) (*       r & s ≈ s + r
                                                         (r & s) & t ≈ r + (s & t) *)
  | _ -> And (List.sort ~cmp:compare [r; s])

let not r =
  match r with
  | Not r -> r (* ¬(¬r) ≈ r *)
  | _ -> Not r

module Infix = struct
  let (++) = cat
  let (||) = either
  let (&&) = both
  let (!)  = not
end

module Chars = struct
  let a = char 'a' let b = char 'b' let c = char 'c' let d = char 'd'
  let e = char 'e' let f = char 'f' let g = char 'g' let h = char 'h'
  let i = char 'i' let j = char 'j' let k = char 'k' let l = char 'l'
  let m = char 'm' let n = char 'n' let o = char 'o' let p = char 'p'
  let q = char 'q' let r = char 'r' let s = char 's' let t = char 't'
  let u = char 'u' let v = char 'v' let w = char 'w' let x = char 'x'
  let y = char 'y' let z = char 'z'
end

let rec to_string r =
  match r with
  | Null -> "∅"
  | Epsilon -> "ε"
  | Char c -> Char.to_string c
  | Cat rs -> String.concat ~sep:"" (List.map rs ~f:to_string)
  | Star (Char c) -> Char.escaped c ^ "*"
  | Star r -> "(" ^ to_string r ^ ")*"
  | Or rs -> "(" ^ String.concat ~sep:"|" (List.map rs ~f:to_string) ^ ")"
  | And rs -> "(" ^ String.concat ~sep:"&" (List.map rs ~f:to_string) ^ ")"
  | Not (Char c) -> "¬" ^ Char.escaped c ^ ")"
  | Not r -> "¬(" ^ to_string r ^ ")"

let v r : t =
  let rec accepts_eps (r: t) : bool =
    match r with
    | Null -> false
    | Epsilon -> true
    | Char _ -> false
    | Cat rs -> List.for_all rs ~f:accepts_eps
    | Star _ -> true
    | Or rs -> List.exists rs ~f:accepts_eps
    | And rs -> List.for_all rs ~f:accepts_eps
    | Not r -> Pervasives.not (accepts_eps r)
  in

  if accepts_eps r
    then epsilon
    else null

let rec dc a r =
  let split r =
    match r with
    | Cat [] | Cat [_] -> failwith "Failed invariant: Cat"
    | Cat [r; s] -> (r, s)
    | Cat (r::rs) -> (r, Cat rs)
    | Or [] | Or [_] -> failwith "Failed invariant: Or"
    | Or [r; s] -> (r, s)
    | Or (r::rs) -> (r, Or rs)
    | And [] | And [_] -> failwith "Failed invariant: And"
    | And [r; s] -> (r, s)
    | And (r::rs) -> (r, And rs)
    | _ -> failwith "failed precondition"
  in

  let open Infix in
  match r with
  | Null -> null
  | Epsilon -> null
  | Char a' when a = a' -> epsilon
  | Char _ -> null
  | Cat _ as r ->
      let (r, s) = split r in
      (dc a r ++ s) || (v r ++ dc a s)
  | Star r -> dc a r ++ star r
  | Or _ as r ->
      let (r, s) = split r in
      dc a r || dc a s
  | And _ as r ->
      let (r, s) = split r in
      dc a r && dc a s
  | Not r -> Not (dc a r)

let d (m: string) (r: t) : t =
  String.fold m ~init:r ~f:(fun r a -> dc a r)

let matches (r: t) (u: string) =
  v (d u r) = epsilon
