open Core.Std
open Async.Std

(* cat [] = ε
 * cat [r_1, r_2, ..., r_n] = Cat(r_1, Cat(r_2, ..., Cat (r_{n-1}, r_n)...) *)
let rec cat rs =
  match rs with
  | [] -> Re.Epsilon
  | [r] -> r
  | r::rs -> Re.Cat (r, cat rs)

let main () =
  let one = Re.Char '1' in
  let zero = Re.Char '0' in
  let zeros = Re.Star zero in

  (* ( 0*10*10* )*: the re that matches bitstrings of even parity *)
  let even = Re.(Star (cat [zeros; one; zeros; one; zeros])) in
  print_endline (Re.show even);

  let stdin = Lazy.force Reader.stdin in
  let lines = Reader.lines stdin in
  print_string "λ ";
  Pipe.iter_without_pushback lines ~f:(fun line ->
    printf "%b\n" (Re.matches even line);
    print_string "λ "
  )

let () =
  Command.(run (async ~summary:"" Spec.empty main))
