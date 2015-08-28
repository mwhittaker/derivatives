open Core.Std

(* extended regular expressions *)
type re =
  | Null           (* ∅     *)
  | Epsilon        (* ε     *)
  | Char of char   (* a     *)
  | Cat of re * re (* rs    *)
  | Star of re     (* r*    *)
  | Or of re * re  (* (r|s) *)
  | And of re * re (* (r&s) *)
  | Not of re      (* ¬r    *)

let rec show (r: re) : string =
  match r with
  | Null -> "∅"
  | Epsilon -> "ε"
  | Char c -> Char.to_string c
  | Cat (r, s) -> (show r) ^ (show s)
  | Star (Char c) -> (Char.to_string c) ^ "*"
  | Star r -> "(" ^ (show r) ^ ")*"
  | Or (r, s) -> "(" ^ (show r) ^ "|" ^ (show s) ^ ")"
  | And (r, s) -> "(" ^ (show r) ^ "&" ^ (show s) ^ ")"
  | Not r -> "¬(" ^ (show r) ^ ")"

(* ν r = ε if r accepts ε, and ∅ otherwise. *)
let rec v (r: re) : re =
  let eps_and r s =
    match r, s with
    | (Epsilon, Epsilon) -> Epsilon
    | _ -> Null
  in

  let eps_or r s =
    match r, s with
    | (Epsilon, Epsilon)
    | (_, Epsilon)
    | (Epsilon, _) -> Epsilon
    | _ -> Null
  in

  let eps_not r =
    match r with
    | Epsilon -> Null
    | Null -> Epsilon
    | _ -> failwith "impossible"
  in

  match r with
  | Null -> Null
  | Epsilon -> Epsilon
  | Char _ -> Null
  | Cat (r, s) -> eps_and (v r) (v s)
  | Star _ -> Epsilon
  | Or (r, s) -> eps_or (v r) (v s)
  | And (r, s) -> eps_and (v r) (v s)
  | Not r -> eps_not (v r)

(* dc a r is ∂ₐ r: the (character) derivative of r with respect to a. *)
let rec dc a r =
  match r with
  | Null -> Null
  | Epsilon -> Null
  | Char a' when a = a' -> Epsilon
  | Char _' -> Null
  | Cat (r, s) -> Or(Cat(dc a r, s), Cat(v r, dc a s))
  | Star r -> Cat(dc a r, Star r)
  | Or (r, s) -> Or(dc a r, dc a s)
  | And (r, s) -> And (dc a r, dc a s)
  | Not r -> Not (dc a r)

(* d m r is ∂ₘ r: the derivative of r with respect to m. *)
let d (m: string) (r: re) : re =
  String.fold m ~init:r ~f:(fun r a -> dc a r)

(* matches r u or (r ~ u) evaluates to true iff r accepts u. *)
let matches (r: re) (u: string) =
  let rec matches' r u =
    match u with
    | [] -> v r = Epsilon
    | a::w -> matches' (dc a r) w
  in

  matches' r (String.to_list u)
