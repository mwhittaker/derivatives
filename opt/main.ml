open Re
open Re.Infix
open Re.Chars

let cat_all rs =
  List.fold_left cat epsilon rs

let main () =
  let r = (a ++ b || a ++ c) in
  print_endline (show r);
  let dfa = Dfa.of_re r in

  let open Core.Std in
  let open Async.Std in
  let stdin = Lazy.force Reader.stdin in
  let lines = Reader.lines stdin in
  print_string "λ ";
  Pipe.iter_without_pushback lines ~f:(fun line ->
    printf "%b\n" (Dfa.matches dfa line);
    print_string "λ "
  )

let () =
  Async.Std.Command.(run (async ~summary:"" Spec.empty main))
