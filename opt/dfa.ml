type state = Re.t

(* Q *)
module States = Set.Make (
  struct
    type t = state
    let compare = compare
  end
)

(* δ *)
module Delta = Map.Make (
  struct
    type t = state * char
    let compare = compare
  end
)

type delta = state Delta.t

type t = {
  states: States.t;
  start: Re.t;
  accepting_states: States.t;
  delta: delta;
}

let alphabet =
  Core.Std.String.to_list @@ String.concat "" [
    "abcdefghijklmnopqrstuvwxyz";
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    "0123456789";
  ]

let to_string {states; start; accepting_states; delta} =
  let states_to_string states =
    States.elements states
    |> List.map Re.to_string
    |> String.concat ", "
    |> fun s -> "(" ^ s ^ ")"
  in

  let delta_to_string delta =
    Delta.bindings delta
    |> List.map (fun ((q, c), q') -> (Re.to_string q, c, Re.to_string q'))
    |> List.map (fun (q, c, q') -> Printf.sprintf "(%s, %c) -> %s" q c q')
    |> String.concat ", "
    |> fun s -> "(" ^ s ^ ")"
  in

  let states_string = "Q = " ^ states_to_string states in
  let start_string = "q0 = " ^ Re.to_string start in
  let accepting_states_string = "F = " ^ states_to_string accepting_states in
  let delta_string = "δ = " ^ delta_to_string delta in

  String.concat "\n" [
    states_string;
    start_string;
    accepting_states_string;
    delta_string;
  ]

let rec goto (q: state) ((states, delta): States.t * delta) (c: char) =
  (*
  let dfa = {
    states = states;
    start = q;
    delta = delta;
    accepting_states = States.empty;
  } in
  print_endline ("\ngoto with DFA \n" ^ (to_string dfa));
  print_endline @@ "c = " ^ Core.Std.Char.to_string c;
  ignore @@ read_line ();
  *)

  let qc = Re.dc c q in
  if States.mem qc states then
    (states, Delta.add (q, c) qc delta)
  else
    let states' = States.add qc states in
    let delta' = Delta.add (q, c) qc delta in
    explore states' delta' qc

and explore (states: States.t) (delta: delta) (q: state) =
  (*
  let dfa = {
    states = states;
    start = q;
    delta = delta;
    accepting_states = States.empty;
  } in
  print_endline ("\nexplore with DFA \n" ^ (to_string dfa));
  ignore @@ read_line ();
  *)

  List.fold_left (goto q) (states, delta) alphabet

let from_re r =
  let start = r in
  let (states, delta) = explore (States.singleton start) (Delta.empty) start in
  let accepting_states = States.filter (fun q -> Re.v q = Re.epsilon) states in
  {states; start; accepting_states; delta}

let transition ({start; delta; _}: t) (s: string) =
  let step (delta: delta) (q: state option) (c: char) =
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
  | Some q -> States.mem q dfa.accepting_states
