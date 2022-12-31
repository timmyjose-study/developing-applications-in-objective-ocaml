let new_s, reset_s =
  let c = ref 0 in
  let f1 s =
    c := !c + 1;
    s ^ string_of_int !c
  and f2 () = c := 0 in
  (f1, f2)