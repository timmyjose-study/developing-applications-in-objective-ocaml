type 'a tree = 
  Empty 
  | Node of 'a * 'a tree list

let rec belongs x = function
  Empty -> false
  | Node (v, _) when x = v -> true
  | Node (_, rest) -> List.exists (belongs x) rest

let rec height =
  let max_list l = List.fold_left max 0 l in
  function
    Empty -> 0
  | Node (_, rest) -> 1 + max_list (List.map height rest)

let t = Node (1, [Node (2, []); Node (3, [Node (4, [])])])

