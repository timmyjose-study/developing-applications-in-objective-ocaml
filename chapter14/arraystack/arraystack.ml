module type ArrStackSig = sig
  type 'a t

  exception Stack_empty

  val create : unit -> 'a t
  val push : 'a -> 'a t -> unit
  val pop : 'a t -> 'a
  val top : 'a t -> 'a
  val length : 'a t -> int
  val iter : ('a -> unit) -> 'a t -> unit
end

module ArrStack : ArrStackSig = struct
  type 'a t = { mutable sp : int; mutable c : 'a array }

  exception Stack_empty

  let size = 5
  let increase s x = s.c <- Array.append s.c (Array.make size x)
  let create () = { sp = 0; c = [||] }

  let clear s =
    s.sp <- 0;
    s.c <- [||]

  let push x s =
    if s.sp >= Array.length s.c then increase s x;
    s.c.(s.sp) <- x;
    s.sp <- succ s.sp

  let pop s =
    if s.sp = 0 then raise Stack_empty
    else
      let ret = s.c.(s.sp - 1) in
      s.sp <- pred s.sp;
      ret

  let top s = if s.sp = 0 then raise Stack_empty else s.c.(s.sp - 1)
  let length s = s.sp

  let iter f s =
    for i = s.sp - 1 downto 0 do
      f s.c.(i)
    done
end