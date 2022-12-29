type coin = Heads | Tails

type suit = Spades | Hearts | Diamonds | Clubs

let string_of_suit = function
  Spades -> "spades"
  | Hearts -> "hearts"
  | Diamonds -> "diamonds"
  | Clubs -> "clubs"

type card = 
  King of suit
  | Queen of suit
  | Knight of suit
  | Knave of suit
  | Minor_card of suit * int
  | Trump of int
  | Joker

let string_of_card = function
  King s -> "King of " ^ (string_of_suit s)
  | Queen s -> "Queen of " ^ (string_of_suit s)
  | Knight s -> "Knight of " ^ (string_of_suit s)
  | Knave s -> "Knave of " ^ (string_of_suit s)
  | Minor_card (s, n) -> string_of_int n  ^ " of " ^ (string_of_suit s)
  | Trump n -> string_of_int n ^ " of Trumps"
  | Joker -> "Joker"

let rec interval a b = 
  if a = b then [b]
  else a :: interval (a + 1) b

let all_cards s = 
  let face_cards = [Knave s; Knight s; Queen s; King s]
  and other_cards = List.map (fun n -> Minor_card (s, n)) (interval 1 10)
  in
  face_cards @ other_cards

let is_minor_card = function
  Minor_card (_, _) -> true
  | _ -> false

(* note the difference - especially important in OCaml compared to, say, Haskell, where type parameters are not parenthesised *)

type foo = 
  C of int * int
  | D of (int * int)

let access_foo = function
  C (a, b) -> (a, b)
  | D x -> x (* or D (c, d) -> (c, d) is fine as well *)

type int_or_char_list = 
  Nil
  | Int_cons of int * int_or_char_list
  | Char_cons of char * int_or_char_list

let l1 = Int_cons (1, Char_cons ('x', Int_cons (2, Char_cons ('y', Int_cons (3, Char_cons ('z', Nil))))))

let rec int_or_char_list_size = function
  Nil -> 0
  | Int_cons (_, t) -> 1 + int_or_char_list_size t
  | Char_cons (_, t) -> 1 + int_or_char_list_size t

(* generalised version of the above *)

type ('a, 'b) list2 = 
  Nil   
  | Acons of 'a * ('a, 'b) list2
  | Bcons of 'b * ('a, 'b) list2

let l2 = Acons (1, Bcons ('x', Acons (2, Bcons ('y', Acons (3, Bcons ('z', Nil))))))

let rec list2_size = function
  Nil -> 0
  | Acons (_, t) | Bcons (_, t) -> 1 + list2_size t

let rec extract_as = function
  Nil -> []
  | Acons (h, t) -> h :: extract_as t
  | Bcons (_, t) -> extract_as t

let rec extract_bs = function
  Nil -> []
  | Acons (_, t) -> extract_bs t
  | Bcons (h, t) -> h :: extract_bs t

(* type of a list of functions, except for the last entry *)

type 'a listf = 
  Val of 'a
  | Fun of ('a -> 'a) * 'a listf

let id x = x 

let sqr x = x * x

let cube x = x * x * x

let apply f x = f x

let rec init = function
  Val _ -> []
  | Fun (f, t) -> f :: init t

let rec last = function
  Val n -> n
  | Fun (_, t) -> last t

let lf = Fun (id, Fun (sqr, Fun (cube, Val 10)))

let rec compute = function
  Val n -> n
  | Fun (f, t) -> f (compute t)