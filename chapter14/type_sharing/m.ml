module type M_SIG = sig
  type t

  val create : unit -> t
  val add : t -> unit
  val get : t -> int
end

module M = struct
  type t = int ref

  let create () = ref 0
  let add x = incr x

  let get x =
    if !x > 0 then (
      decr x;
      1)
    else failwith "Empty"
end

module type S1 = sig
  type t

  val create : unit -> t
  val add : t -> unit
end

(* The `with` syntax is used to make types compatible across abstract types *)

module M1 : S1 with type t = M.t = M

module type S2 = sig
  type t

  val get : t -> int
end

module M2 : S2 with type t = M.t = M
