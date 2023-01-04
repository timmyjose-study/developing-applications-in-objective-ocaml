module type MyStackT = sig
  type 'a t
  exception Stack_empty

  val create : unit -> 'a t
  val clear : 'a t -> unit
  val push : 'a -> 'a t -> unit
  val top : 'a t -> 'a
  val pop : 'a t -> 'a
  val length : 'a t -> int
  val iter : ('a -> unit) -> 'a t -> unit
end