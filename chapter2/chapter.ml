let succ = function x -> x + 1
let pred = function x -> x - 1
let g = function x -> function y -> 2 * x + 3 * y

let succ x = x + 1
let pred x = x - 1

let compose f g x = f (g x)

let rec fact n = if n < 2 then n else n * fact (n - 1)

let fact_of_succ = compose fact succ
let () = assert (fact_of_succ 9 = fact 10)

let succ = ( + ) 1

let ( ++ ) p1 p2 = 
  (fst p1 + fst p2, snd p1 + snd p2)

let rec sigma n = 
  if n = 0 then 0
  else n + sigma (n - 1)

let rec even n = 
  if n = 0 then true
  else odd (n - 1)
and odd n = 
  if n = 0 then false 
  else even (n - 1)

let sigma n = 
  let rec sigma_aux n acc = 
    if n = 0 then acc
    else sigma_aux (n - 1) (acc + n)
  in
  if n < 0 then (failwith "sigma: negative")
  else sigma_aux n 0

let make_pair a b = (a, b)

let app f x = f x 

let id x = x

let rev_app x f = f x

let ( ===> ) = rev_app

let add1 = ( +  ) 1
let mul5 = ( * ) 5
let add1_then_mul5 = compose mul5 add1
let mul5_then_add1 = compose add1 mul5

let add (x : int) (y : int) = x + y

let make_pair_int (x : int) (y : int) = (x, y)

let compose_fn_int (f : int -> int) (g : int -> int) (x : int) = f (g x)

let null l = l = []

let rec range f t = 
  if f = t then [t]
  else f :: range (f + 1) t

let mk_multab n f t = List.map (fun x -> x * n) (range f t)

let rec fold_left f acc l = 
  if null l then acc
  else fold_left f (f acc (List.hd l)) (List.tl l)

let sum n = fold_left ( + ) 0 (range 1 n)
let prod n = fold_left ( * ) 1 (range 1 n)
let concat = fold_left ( @ ) []

