type 'a t

exception Stack_empty

val create : unit -> 'a t
val push : 'a -> 'a t -> unit
val pop : 'a t -> 'a
val top : 'a t -> 'a
val clear : 'a t -> unit
val length : 'a t -> int
val iter : ('a -> unit) -> 'a t -> unit