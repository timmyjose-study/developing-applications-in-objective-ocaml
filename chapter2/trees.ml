(* Binary Search Trees *)

type 'a bin_tree = 
  Empty
  | Node of 'a bin_tree * 'a * 'a bin_tree

let rec insert x = function
  Empty -> Node (Empty, x, Empty)
  | Node (l, v, r) when x <= v -> Node (insert x l, v, r)
  | Node (l, v, r) -> Node (l, v, insert x r)

let rec tree_of_list = function
  [] -> Empty
  | h :: t -> insert h (tree_of_list t)

let rec list_of_tree = function
  Empty -> []
  | Node (l, v, r) -> list_of_tree l @ [v] @ list_of_tree r

let sort l = list_of_tree (tree_of_list l)
