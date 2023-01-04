external factorial : int -> int = "factorial";;

let main () = 
let n = read_int () in
Printf.printf "Factorial(%i) = %i\n" n (factorial n)
