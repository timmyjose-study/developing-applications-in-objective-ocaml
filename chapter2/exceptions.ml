let rec head = function
  [] -> failwith "Empty list"
  | h :: _ -> h

let rec interval a b = if a = b then [b] else a :: interval (a + 1) b

exception My_exn
exception Depth of int

exception Found_zero

let rec mult_rec = function
  [] -> 1
  | 0 :: _ -> raise Found_zero
  | n :: rest -> n * mult_rec rest

let mult_list l = 
  try mult_rec l
  with
  Found_zero -> 0


