module ReSet = Set.Make (
  struct
    type t = Re.t
    let compare = compare
  end
)

module Delta = Map.Make (
  struct
    type t = Re.t * char
    let compare = compare
  end
)

type delta = Re.t Delta.t

type t = {
  states: ReSet.t;
  start: Re.t;
  accepting_states: ReSet.t;
  delta: delta;
}

let rec goto q (states, delta) c =
  let qc = Re.dc c q in
  if ReSet.mem qc states then
    (states, Delta.add (q, c) qc delta)
  else
    let states' = ReSet.add qc states in
    let delta' = Delta.add (q, c) qc delta in
    explore states' delta' qc

and explore states delta q =
  let alphabet = Core.Std.String.to_list "01abc" in
  List.fold_left (goto q) (states, delta) alphabet

let of_re r =
  let start = r in
  let delta = Delta.empty in
  let (states, delta) = explore (ReSet.singleton start) delta start in
  let accepting_states = ReSet.filter (fun q -> Re.v q = Re.epsilon) states in
  {states; start; accepting_states; delta}

let transition {start; delta; accepting_states; _} s =
  let step delta q c =
    let open Core.Std.Option in
    q >>= fun q ->
    if Delta.mem (q, c) delta
      then Some (Delta.find (q, c) delta)
      else None
  in
  List.fold_left (step delta) (Some start) (Core.Std.String.to_list s)

let matches dfa s =
  match transition dfa s with
  | None -> false
  | Some q -> ReSet.mem q dfa.accepting_states
