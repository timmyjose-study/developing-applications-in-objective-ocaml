module type OKEY = sig
  type t

  val create : unit -> t
  val of_string : string -> t
  val to_string : t -> string
  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val gt : t -> t -> bool
end

module Date : OKEY = struct
  type t = float

  let create () = Unix.time ()
  let of_string = float_of_string
  let to_string = string_of_float
  let eq = ( = )
  let lt = ( < )
  let gt = ( > )
end

module type LOG = sig
  type tkey
  type tinfo
  type t

  val create : unit -> t
  val add : tkey -> tinfo -> t -> unit
  val nth : int -> t -> tkey * tinfo
  val get : (tkey -> bool) -> t -> (tkey * tinfo) list
end

module Flog (K : OKEY) = struct
  type tkey = K.t
  type tinfo = float
  type t = { mutable contents : (tkey * tinfo) list }

  let create () = { contents = [] }
  let add k v l = l.contents <- (k, v) :: l.contents
  let nth n l = List.nth l.contents n
  let get f l = List.filter (fun (k, _) -> f k) l.contents
end