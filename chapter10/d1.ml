(*
    check dependencies using:

    $ ocamldep d1.ml
*)
let init n e =
  let v = Array.make n e in
  Dp.print_vect v;
  v
