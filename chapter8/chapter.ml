(* Utilities *)

Random.self_init ()

let random_demo () =
  for i = 1 to 10 do
    Printf.printf "%d %.5f" (Random.int 99 + 1) (Random.float 1.)
  done;
  print_newline ()