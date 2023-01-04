module Couple =
functor
  (Q : sig
     type t
   end)
  ->
  struct
    type couple = Q.t * Q.t
  end

module CoupleAgain (Q : sig
  type t
end) =
struct
  type couple = Q.t * Q.t
end

(* Functors and code reuse *)

module OrderedIntPair = struct
  type t = int * int

  let compare (f1, s1) (f2, s2) =
    if f1 < f2 then -1
    else if f1 > f2 then 1
    else if s1 < s2 then -1
    else if s1 > s2 then 1
    else 0
end

(* Map.Make is a functor *)
module AssocIntPair = Map.Make (OrderedIntPair)
module SetIntPair = Set.Make (OrderedIntPair)
module SetOfSet = Set.Make (SetIntPair)

module IntMod13 = struct
  type t = int

  let equal = ( = )
  let hash x = x mod 13
end

module TblInt = Hashtbl.Make (IntMod13)
