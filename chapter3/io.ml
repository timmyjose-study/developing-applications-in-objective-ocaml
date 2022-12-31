(* guessing game *)

let get_random low high = Random.int (high - low) + low

let rec play secret =
  let () = print_string "What is your guess? " in
  let guess = read_int () in
  if guess < secret then
    let () = print_endline "Too low! Try again.." in
    play secret
  else if guess > secret then
    let () = print_endline "Too high! Try again.." in
    play secret
  else
    let () = print_endline "Correct!" in
    ()

let game () =
  let secret = get_random 1 100 in
  play secret