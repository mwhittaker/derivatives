open Core.Std
open Async.Std
open Re
open Re.Infix
open Re.Chars

let main () =
  (* ( 0*10*10* )* : the re that matches bitstrings of even parity *)
  let one = char '1' in
  let zero = char '0' in
  let zeros = star zero in
  let even = star (zeros ++ one ++ zeros ++ one ++ zeros) in
  let dfa = Dfa.from_re even in

  let stdin = Lazy.force Reader.stdin in
  let lines = Reader.lines stdin in
  print_endline (to_string even);
  print_string "λ ";
  Pipe.iter_without_pushback lines ~f:(fun line ->
    printf "%b\n" (Dfa.matches dfa line);
    print_string "λ "
  )


let () =
  Command.(run (async ~summary:"" Spec.empty main))
