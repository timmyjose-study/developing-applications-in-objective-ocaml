(* We can simulate laziness using thunks *)

type 'a v = Imm of 'a | Deferred of (unit -> 'a)
type 'a vm = { mutable c : 'a v }

(* this is incorrect in the presence of side-dffects *)
let eager_if c e1 e2 = if c then e1 else e2

let eager_if_demo () =
  let x = read_int () in
  eager_if (x > 10) (Printf.printf "One\n") (Printf.printf "Two\n")

(* this is correct, even in the presence of side-effects *)

let eval e =
  match e.c with
  | Imm a -> a
  | Deferred f ->
      let res = f () in
      e.c <- Imm res;
      res

let lazy_if c e1 e2 = if eval c then eval e1 else eval e2

let lazy_if_demo () =
  let x = read_int () in
  lazy_if
    { c = Deferred (function () -> x > 10) }
    { c = Deferred (function () -> Printf.printf "One\n") }
    { c = Deferred (function () -> Printf.printf "Two\n") }

(* lazy factorial *)

let rec factorial n =
  lazy_if
    { c = Deferred (function () -> n = 0) }
    { c = Deferred (function () -> 1) }
    { c = Deferred (function () -> n * factorial (n - 1)) }

(* The Lazy module *)

let lazy_demo () =
  let x =
    lazy
      (print_string "Ni hao";
       (* only printed once *)
       3 + 4)
  in
  for i = 1 to 10 do
    ignore (Lazy.force x)
  done

(* infinite data structures *)

type 'a enum = { mutable i : 'a; f : 'a -> 'a }

let next e =
  let x = e.i in
  e.i <- e.f e.i;
  x

let nat = { i = 0; f = (function x -> x + 1) }

let fib =
  let fx =
    let c = ref 0 in
    function
    | v ->
        let r = !c + v in
        c := v;
        r
  in
  { i = 1; f = fx }

(* Streams - TODO once camplp5 is mastered *)