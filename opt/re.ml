(* extended regular expressions *)
type t =
  | Null          (* ∅     *)
  | Epsilon       (* ε     *)
  | Char of char  (* a     *)
  | Cat of t list (* r·s   *)
  | Star of t     (* r*    *)
  | Or of t list  (* (r|s) *)
  | And of t list (* (r&s) *)
  | Not of t      (* ¬r    *)

let null = Null
let epsilon = Epsilon
let char c = Char c

let cat r s =
  match r, s with
  | Null, _ -> Null             (*     ∅·r ≈ ∅       *)
  | r, Null -> r                (*     r·∅ ≈ ∅       *)
  | Epsilon, r -> r             (*     ε·r ≈ ε       *)
  | r, Epsilon -> r             (*     r·ε ≈ ε       *)
  | Cat rs, s -> Cat (rs @ [s]) (* (r·s)·t ≈ r·(s·t) *)
  | _ -> Cat [r; s]

let star r =
  match r with
  | Star r -> Star r   (* ( r* )* ≈ r *)
  | Epsilon -> Epsilon (*      ε* ≈ ε *)
  | Null -> Epsilon    (*      ∅* ≈ ε *)
  | _ -> Star r

let either r s =
  match r, s with
  | Or rs, r when List.mem r rs -> Or rs       (*       r + r ≈ r           *)
  | Or rs, r -> Or (List.merge compare rs [r]) (*       r + s ≈ s + r
                                                  (r + s) + t ≈ r + (s + t) *)
  | Not Null, _ -> Not Null                    (*      ¬∅ + r ≈ ¬∅          *)
  | Null, r -> r                               (*       ∅ + r ≈ ∅           *)
  | _, Not Null -> Not Null                    (*      ¬∅ + r ≈ ¬∅          *)
  | r, Null -> r                               (*       ∅ + r ≈ ∅           *)
  | _ -> Or (List.sort compare [r; s])

let both r s =
  match r, s with
  | And rs, r when List.mem r rs -> And rs       (*       r & r ≈ r           *)
  | And rs, r -> And (List.merge compare rs [r]) (*       r & s ≈ s + r
                                                    (r & s) & t ≈ r + (s & t) *)
  | Null, _ -> Null                              (*       ∅ & r ≈ ∅           *)
  | Not Null, r -> r                             (*      ¬∅ & r ≈ ∅           *)
  | _, Null -> Null                              (*       ∅ & r ≈ ∅           *)
  | r, Not Null -> r                             (*      ¬∅ & r ≈ ∅           *)
  | _ -> And (List.sort compare [r; s])

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
  let a = char 'a'
  let b = char 'b'
  let c = char 'c'
  let d = char 'd'
  let e = char 'e'
  let f = char 'f'
  let g = char 'g'
  let h = char 'h'
  let i = char 'i'
  let j = char 'j'
  let k = char 'k'
  let l = char 'l'
  let m = char 'm'
  let n = char 'n'
  let o = char 'o'
  let p = char 'p'
  let q = char 'q'
  let r = char 'r'
  let s = char 's'
  let t = char 't'
  let u = char 'u'
  let v = char 'v'
  let w = char 'w'
  let x = char 'x'
  let y = char 'y'
  let z = char 'z'
end

let rec show r =
  match r with
  | Null -> "∅"
  | Epsilon -> "ε"
  | Char c -> Core.Std.Char.to_string c
  | Cat rs -> String.concat "" (List.map show rs)
  | Star (Char c) -> (Char.escaped c) ^ "*"
  | Star r -> "(" ^ (show r) ^ ")*"
  | Or rs -> "(" ^ String.concat "|" (List.map show rs) ^ ")"
  | And rs -> "(" ^ String.concat "&" (List.map show rs) ^ ")"
  | Not r -> "¬(" ^ show r ^ ")"

(* ν r = ε if r accepts ε, and ∅ otherwise. *)
let v (r: t) : t =
  let rec accepts_null (r: t) : bool =
    match r with
    | Null -> false
    | Epsilon -> true
    | Char _ -> false
    | Cat rs -> List.for_all accepts_null rs
    | Star _ -> true
    | Or rs -> List.exists accepts_null rs
    | And rs -> List.for_all accepts_null rs
    | Not r -> Pervasives.not (accepts_null r)
  in

  if accepts_null r
    then epsilon
    else null


(* dc a r is ∂ₐ r: the (character) derivative of r with respect to a. *)
let rec dc a r =
  let split r =
    match r with
    | Cat [r; s] -> (r, s)
    | Cat (r::rs) -> (r, Cat rs)
    | Or [r; s] -> (r, s)
    | Or (r::rs) -> (r, Or rs)
    | And [r; s] -> (r, s)
    | And (r::rs) -> (r, And rs)
    | _ -> failwith "failed precondition"
  in

  match r with
  | Null -> null
  | Epsilon -> null
  | Char a' when a = a' -> epsilon
  | Char _' -> null
  | Cat _ as r ->
      let (r, s) = split r in
      Infix.((dc a r ++ s) || v r ++ dc a s)
  | Star r -> Infix.(dc a r ++ star r)
  | Or _ as r ->
      let (r, s) = split r in
      Infix.(dc a r || dc a s)
  | And _ as r ->
      let (r, s) = split r in
      Infix.(dc a r && dc a s)
  | Not r -> Not (dc a r)

(* d m r is ∂ₘ r: the derivative of r with respect to m. *)
let d (m: string) (r: t) : t =
  Core.Std.String.fold m ~init:r ~f:(fun r a -> dc a r)

(* matches r u or (r ~ u) evaluates to true iff r accepts u. *)
let matches (r: t) (u: string) =
  v (d u r) = epsilon
