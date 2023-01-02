(* Utilities *)

Random.self_init ()

let random_demo () =
  for i = 1 to 10 do
    Printf.printf "%d %.5f" (Random.int 99 + 1) (Random.float 1.)
  done;
  print_newline ()

let rec interval a b = if a = b then [ b ] else a :: interval (a + 1) b

let print_content iter print_item xs =
  iter
    (fun x ->
      print_string "(";
      print_item x;
      print_string ")")
    xs;
  print_newline ()

let for_all p = List.fold_left (fun acc x -> acc && p x) true
let exists p = List.fold_left (fun acc x -> acc || p x) true

let matrix_demo () =
  let print_matrix m =
    Printf.printf "\n";
    for i = 0 to Array.length m - 1 do
      for j = 0 to Array.length m.(0) - 1 do
        Printf.printf "%10.5f" m.(i).(j)
      done;
      Printf.printf "\n"
    done
  and mat = Array.make_matrix 2 3 (Random.float 1.) in
  print_matrix mat

(* #load "nums.cma" *)
let rec fact_num n =
  if Num.( <=/ ) n (Num.Int 0) then Num.Int 1
  else Num.( */ ) n (fact_num (Num.( -/ ) n (Num.Int 1)))

let calc_e m =
  let a = Num.( +/ ) (Num.Int 1) (Num.( // ) (Num.Int 1) m) in
  Num.( **/ ) a m