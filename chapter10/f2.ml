(*
  $ ocamlcp -p a -o f2 f1.cmp f2.ml
  $ ocamlprof f1.ml
*)
let l1 = F1.interval 1 30
let l2 = F1.interval 31 60
let l3 = l1 @ (0 :: l2);;

print_int (F1.mult_list l1);
print_newline ();
print_int (F1.mult_list l2);
print_newline ()