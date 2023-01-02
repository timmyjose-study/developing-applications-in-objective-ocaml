(*
    debug for ocamlopt:
   $ ocamlopt -runtime-variant d -x -g -o fact fact.ml main.ml
*)
let go () =
  let n = read_int () in
  Printf.printf "%i\n" (Fact.fact n)
;;

go ()