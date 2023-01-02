(*
 $ ocamlcp -p a -c f1.ml or
*)
let rec interval a b = if b < a then [] else a :: interval (a + 1) b

exception Found_zero

let mult_list l =
  let rec mult_list_aux l acc =
    match l with
    | [] -> 1
    | 0 :: _ -> raise Found_zero
    | h :: t -> mult_list_aux t (acc * h)
  in
  try mult_list_aux l 1 with Found_zero -> 0