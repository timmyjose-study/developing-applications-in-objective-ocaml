type 'a t = { mutable c : 'a list }

exception Stack_empty

let create () = { c = [] }
let push x s = s.c <- x :: s.c

let pop s =
  if s.c = [] then raise Stack_empty
  else
    let ret = List.hd s.c in
    s.c <- List.tl s.c;
    ret

let top s = if s.c = [] then raise Stack_empty else List.hd s.c
let clear s = s.c <- []
let length s = List.length s.c
let iter f s = List.iter f s.c
