let arr1 = Array.make 10 0
let arr2 = Array.make 10 0.
let arr3 = Array.create_float 20
let mat1 = Array.make_matrix 2 3 0

(* non-reactangular matrix *)

let t =
  [|
    [| 1 |];
    [| 1; 2; 1 |];
    [| 1; 3; 3; 1 |];
    [| 1; 4; 6; 4; 1 |];
    [| 1; 5; 10; 10; 5; 1 |];
  |]

(* strings are also basically characters array *)

let char_arr_demo () =
  let s = "Hello, world!" in
  let slen = String.length s in
  for idx = 0 to slen - 1 do
    Printf.printf "%c\n" s.[idx]
  done

type point = { mutable xc : float; mutable yc : float }

let moveto p dx dy =
  let () = p.xc <- p.xc +. dx in
  p.yc <- p.yc +. dy

type t = { c1 : int; mutable c2 : int }
type 'a myref = { mutable contents : 'a }

let ( !! ) r = r.contents
let ( >> ) r x = r.contents <- x


