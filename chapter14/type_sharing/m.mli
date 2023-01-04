module type M_SIG = sig
  type t

  val create : unit -> t
  val add : t -> unit
  val get : t -> int
end

module M : M_SIG

module type S1 = sig
  type t

  val create : unit -> t
  val add : t -> unit
end

module M1 : S1 with type t = M.t

module type S2 = sig
  type t

  val get : t -> int
end

module M2 : S2 with type t = M.t