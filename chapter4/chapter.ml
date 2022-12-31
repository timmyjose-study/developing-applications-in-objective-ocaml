type person = { mutable name : string; mutable age : int }

let rec map f = function [] -> [] | h :: t -> f h :: map f t

type 'a ilist = { mutable c : 'a list }

let icreate () = { c = [] }
let iempty l = l.c = []

let icons e l =
  l.c <- e :: l.c;
  l

let ihd l = List.hd l.c

let itl l =
  l.c <- List.tl l.c;
  l

(* order of evaluation of parameters in OCaml is undefined *)
let rec imap f l =
  if iempty l then icreate () else icons (f (ihd l)) (imap f (itl l))

(* potentials for stack overflow *)
let rec succ n = if n = 0 then 1 else 1 + succ (n - 1)

(* no stack overflow, of course *)
let succ_iter n =
  let r = ref 0 in
  for i = 0 to n do
    incr r
  done;
  !r

(* no stack overflow since it's tail recursive *)
let succ_tailrec n =
  let rec succ_tailrec_aux n acc =
    if n = 0 then acc else succ_tailrec_aux (n - 1) (acc + 1)
  in
  succ_tailrec_aux n 1

let nil_assoc = function _ -> raise Not_found
let add_assoc (k, v) l = function x -> if x = k then v else l x
let rem_assoc k l = function x -> if x = k then raise Not_found else l x

let mem_assoc k l =
  try
    l k;
    true
  with Not_found -> false