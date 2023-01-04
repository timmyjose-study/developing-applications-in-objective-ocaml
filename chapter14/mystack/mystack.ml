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

module MyStack : MyStackT = struct
  type 'a t = { mutable c : 'a list }

  exception Stack_empty

  let create () = { c = [] }
  let clear s = s.c <- []
  let push x s = s.c <- x :: s.c
  let top s = if s.c = [] then raise Stack_empty else List.hd s.c
  let pop s = if s.c = [] then raise Stack_empty 
  else let ret = List.hd s.c in 
  (s.c <- List.tl s.c;
  ret)
  let length s = List.length s.c
  let iter f s = List.iter f s.c
end