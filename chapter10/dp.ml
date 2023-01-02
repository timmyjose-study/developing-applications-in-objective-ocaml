let print_vect v =
  for i = 0 to Array.length v - 1 do
    Printf.printf "%f " v.(i)
  done;
  print_newline ()
