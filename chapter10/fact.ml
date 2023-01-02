let fact n =
  let rec fact_aux n acc =
    match n with 0 -> acc | _ -> fact_aux (n - 1) (acc * n)
  in
  let () = assert (n >= 0) in
  fact_aux n 1