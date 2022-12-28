let id = function x -> x 

let compose f g x = f (g x)

let rec iterate n f = 
  if n = 0 then id
  else compose f (iterate (n - 1) f)

let rec power b e = 
  let byb = ( *  ) b in
  iterate e byb 1

